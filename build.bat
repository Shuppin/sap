@echo off
cargo build --release
move .\target\release\cli.exe .\sap.exe