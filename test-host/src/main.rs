use extism::*;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let wasm = std::fs::read(&args[0]).unwrap();
    let manifest = Manifest::new([wasm]).with_config_key("greeting", "Hi there");
    let mut plugin = Plugin::new(manifest, [], true).unwrap();
    let res: String = plugin.call(&args[1], &args[2]).unwrap();
    println!("{}", res);
}
