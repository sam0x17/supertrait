use quote::ToTokens;
use std::{
    collections::HashSet,
    fmt::Debug,
    hash::{Hash, Hasher},
};
use syn::{
    parse2,
    visit::{self, Visit},
    AngleBracketedGenericArguments, Expr, GenericParam, Generics, Lifetime, PathArguments,
    PredicateType, ReturnType, Signature, TraitBound, Type, TypeBareFn, TypeImplTrait,
    TypeParamBound, TypeParen, TypePath, TypeReference, TypeTraitObject,
};

#[derive(Clone)]
pub enum GenericUsage {
    Type(TypePath),
    Lifetime(Lifetime),
    Const(Expr),
}

impl From<GenericParam> for GenericUsage {
    fn from(value: GenericParam) -> Self {
        (&value).into()
    }
}

impl From<&GenericParam> for &GenericUsage {
    fn from(value: &GenericParam) -> Self {
        value.into()
    }
}

impl From<&GenericParam> for GenericUsage {
    fn from(value: &GenericParam) -> Self {
        match value {
            GenericParam::Type(type_param) => GenericUsage::from_type(&type_param.ident),
            GenericParam::Lifetime(lifetime_def) => {
                GenericUsage::from_lifetime(&lifetime_def.lifetime)
            }
            GenericParam::Const(const_param) => GenericUsage::from_const(&const_param.ident),
        }
    }
}

impl Debug for GenericUsage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type(typ) => f.debug_tuple("Type").field(&typ.to_token_stream()).finish(),
            Self::Lifetime(life) => f
                .debug_tuple("Lifetime")
                .field(&life.to_token_stream())
                .finish(),
            Self::Const(constant) => f
                .debug_tuple("Const")
                .field(&constant.to_token_stream())
                .finish(),
        }
    }
}

impl Hash for GenericUsage {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let discriminant = core::mem::discriminant(self);
        match self {
            GenericUsage::Type(typ) => {
                (discriminant, typ.to_token_stream().to_string()).hash(state)
            }
            GenericUsage::Lifetime(lifetime) => {
                (discriminant, lifetime.to_token_stream().to_string()).hash(state)
            }
            GenericUsage::Const(constant) => {
                (discriminant, constant.to_token_stream().to_string()).hash(state)
            }
        }
    }
}

impl PartialEq for GenericUsage {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Type(l), Self::Type(r)) => {
                l.to_token_stream().to_string() == r.to_token_stream().to_string()
            }
            (Self::Lifetime(l), Self::Lifetime(r)) => {
                l.to_token_stream().to_string() == r.to_token_stream().to_string()
            }
            (Self::Const(l), Self::Const(r)) => {
                l.to_token_stream().to_string() == r.to_token_stream().to_string()
            }
            _ => false,
        }
    }
}

impl Eq for GenericUsage {}

impl GenericUsage {
    pub fn from_type(input: impl ToTokens) -> Self {
        GenericUsage::Type(parse2::<TypePath>(input.to_token_stream()).unwrap())
    }

    pub fn from_const(input: impl ToTokens) -> Self {
        GenericUsage::Const(parse2::<Expr>(input.to_token_stream()).unwrap())
    }

    pub fn from_lifetime(input: impl ToTokens) -> Self {
        GenericUsage::Lifetime(parse2::<Lifetime>(input.to_token_stream()).unwrap())
    }
}

pub struct FindGenericParam {
    pub subjects: HashSet<GenericUsage>,
    pub usages: HashSet<GenericUsage>,
}

impl FindGenericParam {
    pub fn new(input_generics: &Generics) -> Self {
        let mut subjects: HashSet<GenericUsage> = HashSet::new();
        for generic_param in &input_generics.params {
            subjects.insert(generic_param.into());
        }

        // NOTE: there is no need for us to also traverse where clause as new generics can't be
        // _introduced_ there

        FindGenericParam {
            subjects,
            usages: HashSet::new(),
        }
    }

    pub fn add_type_usage(&mut self, usage: impl ToTokens) {
        let usage = GenericUsage::from_type(usage);
        if self.subjects.contains(&usage) {
            self.usages.insert(usage);
        }
    }

    pub fn add_const_usage(&mut self, usage: impl ToTokens) {
        let usage = GenericUsage::from_const(usage);
        if self.subjects.contains(&usage) {
            self.usages.insert(usage);
        }
    }

    pub fn add_lifetime_usage(&mut self, usage: impl ToTokens) {
        let usage = GenericUsage::from_lifetime(usage);
        if self.subjects.contains(&usage) {
            self.usages.insert(usage);
        }
    }
}

impl<'ast> Visit<'ast> for FindGenericParam {
    // Covers usage like: Vec<T>
    fn visit_type_path(&mut self, node: &'ast TypePath) {
        for segment in &node.path.segments {
            if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
                &segment.arguments
            {
                for arg in args {
                    if let syn::GenericArgument::Type(Type::Path(type_path)) = arg {
                        self.add_type_usage(type_path);
                    }
                }
            }
        }

        self.add_type_usage(node);

        visit::visit_type_path(self, node);
    }

    // Covers usage like: fn foo<T>(arg: T) -> T {}
    fn visit_signature(&mut self, node: &'ast Signature) {
        for input in &node.inputs {
            if let syn::FnArg::Typed(pat) = input {
                if let Type::Path(type_path) = &*pat.ty {
                    self.add_type_usage(type_path);
                }
            }
        }

        visit::visit_signature(self, node);
    }

    // Covers usage like: where T: SomeTrait
    fn visit_predicate_type(&mut self, node: &'ast PredicateType) {
        if let Type::Path(type_path) = &node.bounded_ty {
            self.add_type_usage(type_path);
        }

        visit::visit_predicate_type(self, node);
    }

    // Covers usage like: T: SomeTrait
    fn visit_type_param_bound(&mut self, node: &'ast TypeParamBound) {
        if let TypeParamBound::Trait(TraitBound { path, .. }) = node {
            if let Some(ident) = path.get_ident() {
                let usage = GenericUsage::from_type(ident);
                if self.subjects.contains(&usage) {
                    self.usages.insert(usage);
                }
            }
        }

        visit::visit_type_param_bound(self, node);
    }

    // Covers usage like: type AssocType: SomeTrait;
    fn visit_type_param(&mut self, node: &'ast syn::TypeParam) {
        for bound in &node.bounds {
            if let syn::TypeParamBound::Trait(trait_bound) = bound {
                if let Some(ident) = trait_bound.path.get_ident() {
                    self.add_type_usage(ident);
                }
            }
        }

        visit::visit_type_param(self, node);
    }

    // Covers usage like: 'a
    fn visit_lifetime(&mut self, lifetime: &'ast Lifetime) {
        self.add_lifetime_usage(lifetime);
        visit::visit_lifetime(self, lifetime);
    }

    // Covers usage like: const GENERIC: Type = value;
    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::Path(expr_path) = expr {
            if let Some(ident) = expr_path.path.get_ident() {
                self.add_const_usage(ident);
            }
        }
        visit::visit_expr(self, expr);
    }

    // Covers usage like: dyn SomeTrait<T>
    fn visit_type_trait_object(&mut self, node: &'ast TypeTraitObject) {
        for bound in &node.bounds {
            if let TypeParamBound::Trait(TraitBound { path, .. }) = bound {
                if let Some(ident) = path.get_ident() {
                    self.add_type_usage(ident);
                }
            }
        }

        visit::visit_type_trait_object(self, node);
    }

    // Covers usage like: fn(T)
    fn visit_type_bare_fn(&mut self, node: &'ast TypeBareFn) {
        for arg in &node.inputs {
            if let Type::Path(type_path) = &arg.ty {
                self.add_type_usage(type_path);
            }
        }

        if let ReturnType::Type(_, typ) = &node.output {
            self.add_type_usage(typ);
        }

        visit::visit_type_bare_fn(self, node);
    }

    // Covers usage like: (T)
    fn visit_type_paren(&mut self, node: &'ast TypeParen) {
        if let Type::Path(type_path) = &*node.elem {
            self.add_type_usage(type_path);
        }

        visit::visit_type_paren(self, node);
    }

    // Covers usage like: impl SomeTrait<T>
    fn visit_type_impl_trait(&mut self, node: &'ast TypeImplTrait) {
        for bound in &node.bounds {
            if let TypeParamBound::Trait(TraitBound { path, .. }) = bound {
                if let Some(ident) = path.get_ident() {
                    self.add_type_usage(ident);
                }
            }
        }

        visit::visit_type_impl_trait(self, node);
    }

    // Covers usage like: &T and &'a T
    fn visit_type_reference(&mut self, node: &'ast TypeReference) {
        if let Some(ref lifetime) = node.lifetime {
            self.add_lifetime_usage(lifetime);
        }

        if let Type::Path(type_path) = &*node.elem {
            self.add_type_usage(type_path);
        }

        visit::visit_type_reference(self, node);
    }
}
