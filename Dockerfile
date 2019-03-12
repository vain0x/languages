FROM rust:1.33.0
RUN rustup update stable

COPY . /picomet-lang

WORKDIR /picomet-lang/compiler
RUN cargo build --all

WORKDIR /picomet-lang/solver
RUN cargo build
