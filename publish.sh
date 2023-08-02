#!/bin/sh
cargo test --workspace || exit 1
cd macros || exit 1
cargo publish || exit 1
cd .. || exit 1
cargo publish || exit 1
echo "done"
