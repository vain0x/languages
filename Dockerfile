FROM rust:latest

COPY . /picomet-lang

WORKDIR /picomet-lang/solver
RUN cargo build
