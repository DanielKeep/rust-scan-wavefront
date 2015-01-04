extern crate "rustc-serialize" as rustc_serialize;
extern crate scan_obj;

fn main() {
	let args = std::os::args();
	if args.len() != 2 {
		println!("usage: obj2json FILE");
		std::os::set_exit_status(1);
		return;
	}

	let arg_file = args[1].as_slice();

	let mut file_rd = std::io::fs::File::open(&std::path::Path::new(arg_file)).unwrap();
	let mut obj_stmts = vec![];

	for stmt in scan_obj::ObjScanner::new(&mut file_rd) {
		obj_stmts.push(stmt.unwrap());
	}

	let jsoned = rustc_serialize::json::encode(&obj_stmts);
	print!("{}", jsoned);
}
