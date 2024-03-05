@echo off
cargo build --release
move .\target\release\sap.exe .\sap.exe