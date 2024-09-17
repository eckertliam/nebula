mod frontend;
// the nir (pronounced "near") module is the intermediate representation of nebula
mod nir;
// passes done over the nir
mod passes;
// the backend module provides an api to generate code from the nir
mod backend;

fn main() {
    println!("Hello, world!");
}
