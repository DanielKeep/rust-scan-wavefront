# rust-scan-wavefront

This is a simple Wavefront OBJ scanner, built as a test for the [`rust-scan`](https://github.com/DanielKeep/rust-scan.git) library.

It currently scans most of a standard version 3.0 OBJ file, with the exception of the superceded directives and anything to do with `call`, `csh` or variable substitution, because I couldn't find any substantiative documentation for them.

It does *not* currently scan MTL files.

This package also includes a `obj2json` test program which simply dumps the result of the scan through Rust's automatic JSON serialisation.

## Usage

To use this, add the following to your project's `Cargo.toml` file:

	[dependencies.scan_obj]

	git = "https://github.com/DanielKeep/rust-scan-wavefront.git"

The OBJ scanner can be used as an iterator.  Given `rd : Reader`:

	use scan_obj::{ObjScanner, ObjVertex, ObjFace};

	for stmt in scan_obj::ObjScanner::new(&mut rd) {
		// stmt is a Result<scan_obj::ObjStatement, _>
		match stmt.unwrap() {
			ObjVertex(x, y, z, w) => ...
			ObjFace(vs) => ...
		}
	}
