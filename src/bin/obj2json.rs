#![allow(non_snake_case)]
#![feature(phase)]

extern crate serialize;

#[phase(plugin)] extern crate docopt_macros;
extern crate docopt;
extern crate scan_obj;

use docopt::FlagParser;

docopt! { Args, "
Usage: obj2json FILE
"}

fn main() {
	let args: Args = FlagParser::parse().unwrap_or_else(|e| e.exit());

	let mut file_rd = std::io::fs::File::open(&std::path::Path::new(args.arg_FILE.as_slice())).unwrap();
	let mut obj_stmts = vec![];

	for stmt in scan_obj::ObjScanner::new(&mut file_rd) {
		obj_stmts.push(stmt.unwrap());
	}

	let jsoned = serialize::json::encode(&obj_stmts);
	print!("{}", jsoned);
}
