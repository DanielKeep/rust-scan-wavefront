/*!
This library implements a simple Wavefront OBJ scanner, built as a test for the [`rust-scan`](https://github.com/DanielKeep/rust-scan.git) library.  It is based on the ASCII copy of the OBJ specification at <http://www.martinreddy.net/gfx/3d/OBJ.spec>.

The short version: create an `ObjScanner` from a `Reader`, then iterate over it to get the scanned statements.  For example, here's how you might extract all the vertices from an OBJ file, ignoring everything else.

```
# extern crate scan_obj;
use std::io::BufReader;
use scan_obj::{ObjScanner, ObjVertex};

fn parse_verts<R: Reader>(rd: &mut R) -> Vec<(f64, f64, f64)> {
	let mut verts = vec![];
	for stmt in ObjScanner::new(rd).map(|e| e.unwrap()) {
		match stmt {
			ObjVertex(x, y, z, _) => verts.push((x, y, z)),
			_ => ()
		}
	}
	verts
}

fn main() {
	let verts = parse_verts(&mut BufReader::new(r#"
		o Cube
		v 1.000000 -1.000000 -1.000000
		v 1.000000 -1.000000 1.000000
		v -1.000000 -1.000000 1.000000
		v -1.000000 -1.000000 -1.000000
		v 1.000000 1.000000 -1.000000
		v 1.000000 1.000000 1.000000
		v -1.000000 1.000000 1.000000
		v -1.000000 1.000000 -1.000000
		s off
		f 1 2 3 4
		f 5 8 7 6
		f 1 5 6 2
		f 2 6 7 3
		f 3 7 8 4
		f 5 1 4 8
	"#.as_bytes()));

	assert_eq!(verts, vec![
		( 1.0, -1.0, -1.0),
		( 1.0, -1.0,  1.0),
		(-1.0, -1.0,  1.0),
		(-1.0, -1.0, -1.0),
		( 1.0,  1.0, -1.0),
		( 1.0,  1.0,  1.0),
		(-1.0,  1.0,  1.0),
		(-1.0,  1.0, -1.0),
	]);
}
```
*/
#![feature(globs)]
#![feature(if_let)]
#![feature(phase)]

extern crate serialize;
#[phase(plugin)] extern crate scan;
extern crate scan_util;

pub use self::ObjStatement::*;
pub use self::IsRational::{Rational, Irrational};
pub use self::CSType::{BasisMatrix, Bezier, BSpline, Cardinal, Taylor};
pub use self::Direction::{InU, InV};
pub use self::CurveApprox::{CurveConstParam, CurveConstSpace, CurveCurvatureDep};
pub use self::SurfaceApprox::{SurfConstParam, SurfConstSpace, SurfCurvatureDep};

use std::io::IoResult;
use scan_util::{ScanResult, ScanIoError};

/**
This structure provides a simple interface to the OBJ scanner.  Just create one with a `&mut Reader` and then iterate over it.  It will cease yielded new values once a `EndOfFile` error is encountered.

Note that each element in the sequence is a `Result<ObjStatement, scan_util::ScanError>`.  The simplest thing to do is to just `unwrap` the error.  If you want to actually handle the errors, you will probably want to depend on the `scan_util` crate.
*/
pub struct ObjScanner<'a, R: 'a + Reader> {
	rd: &'a mut R,
}

impl<'a, R: 'a + Reader> ObjScanner<'a, R> {
	/**
Create a new `ObjScanner` with the given `Reader`.
	*/
	pub fn new(rd: &'a mut R) -> ObjScanner<'a, R> {
		ObjScanner {
			rd: rd,
		}
	}
}

impl<'a, R: 'a + Reader> Iterator<ScanResult<ObjStatement>> for ObjScanner<'a, R> {
	/**
Yield the next `ObjStatement`.  If an IO or scanning error occurs, you will get an `Err(scan_util::ScanError)`, otherwise you will get an `Ok(ObjStatement)`.  In the case of an `EndOfFile` IO error, this method will just return `None`.
	*/
	fn next(&mut self) -> Option<ScanResult<ObjStatement>> {
		loop {
			let line = match read_obj_line(self.rd) {
				Err(err) => {
					if err.kind == std::io::EndOfFile {
						return None
					} else {
						return Some(Err(ScanIoError(err)))
					}
				},
				Ok(line) => line
			};
			match scan_obj_line(line.as_slice()) {
				Ok(Some(stmt_result)) => return Some(Ok(stmt_result)),
				Err(err) => return Some(Err(err)),
				Ok(None) => ()
			}
		}
	}
}

/**
This enumeration represents every supported OBJ statement.  Each variant is a direct translation from the file.  This means, for example, that vertex references are raw integers and are not checked for validity.
*/
#[deriving(Decodable, Encodable, PartialEq, Show)]
pub enum ObjStatement {
	ObjVertex(f64, f64, f64, f64),
	ObjVertexTexture(f64, f64, f64),
	ObjVertexNormal(f64, f64, f64),
	ObjVertexParam(f64, f64, f64),
	ObjCSType(IsRational, CSType),
	ObjDegree(f64, f64),
	ObjBasisMatrix(Direction, Vec<f64>),
	ObjStepSize(f64, Option<f64>),

	ObjPoints(Vec<DataRef>),
	ObjLine(Vec<DataPair>),
	ObjFace(Vec<DataTriple>),
	ObjCurve(f64, f64, Vec<DataRef>),
	Obj2dCurve(Vec<DataRef>),
	ObjSurface(f64, f64, f64, f64, Vec<DataTriple>),

	ObjParam(Direction, Vec<f64>),
	ObjTrim(Vec<CurveRef>),
	ObjHole(Vec<CurveRef>),
	ObjSpecialCurve(Vec<CurveRef>),
	ObjSpecialPoint(Vec<DataRef>),
	ObjEnd,

	ObjConnect(SurfaceRef, SurfaceRef),

	ObjGroups(Vec<String>),
	ObjSmoothingGroup(uint),
	ObjMergingGroup(uint, Option<f64>),
	ObjObjectName(String),

	ObjBevelInterp(bool),
	ObjColorInterp(bool),
	ObjDissolveInterp(bool),
	ObjLOD(uint),
	ObjTexMapLibraries(Vec<String>),
	ObjTexMap(Option<String>),
	ObjMtl(String),
	ObjMtlLibraries(Vec<String>),
	ObjShadowCaster(String),
	ObjReflectionObj(String),
	ObjCurveApprox(CurveApprox),
	ObjSurfaceApprox(SurfaceApprox),
}

/**
Alias for the type of internal references to vertices, curves, etc.
*/
pub type DataRef = int;

/**
Is a curve or surface rational?
*/
#[deriving(Decodable, Encodable, Eq, PartialEq, Show)]
pub enum IsRational {
	Rational,
	Irrational,
}

scanner! { IsRational,
	"rat" => Rational,
	_ignore:() => Irrational,
}

/**
The type of curve or surface.
*/
#[deriving(Decodable, Encodable, Eq, PartialEq, Show)]
pub enum CSType {
	BasisMatrix,
	Bezier,
	BSpline,
	Cardinal,
	Taylor,
}

scanner! { CSType,
	"bmatrix" => BasisMatrix,
	"bezier" => Bezier,
	"bspline" => BSpline,
	"cardinal" => Cardinal,
	"taylor" => Taylor,
}

/**
This is used to encode the direction for basis matrices and parameters.
*/
#[deriving(Decodable, Encodable, Eq, PartialEq, Show)]
pub enum Direction {
	InU,
	InV,
}

scanner! { Direction,
	"u" => InU,
	"v" => InV,
}

/**
A pair of references, used to specify a vertex and texture coordinate.
*/
#[deriving(Decodable, Encodable, Eq, PartialEq, Show)]
pub struct DataPair {
	pub v_ref: DataRef,
	pub vt_ref: Option<DataRef>,
}

scanner! { DataPair,
	// TODO: forbid whitespace inside the pair (before is OK).
	v ("/" vt)? => DataPair {
		v_ref: v,
		vt_ref: vt,
	}
}

/**
A triple of references, used to specify a vertex, texture coordinate and vertex normal.
*/
#[deriving(Decodable, Encodable, Eq, PartialEq, Show)]
pub struct DataTriple {
	pub v_ref: DataRef,
	pub vt_ref: Option<DataRef>,
	pub vn_ref: Option<DataRef>,
}

scanner! { DataTriple,
	// TODO: forbid whitespace inside the triplet (before is OK).
	v "/" vt? "/" vn => DataTriple {
		v_ref: v,
		vt_ref: vt,
		vn_ref: Some(vn),
	},
	v ("/" vt)? => DataTriple {
		v_ref: v,
		vt_ref: vt,
		vn_ref: None,
	}
}

/**
A reference to a curve, along with start and end parameters.
*/
#[deriving(Decodable, Encodable, PartialEq, Show)]
pub struct CurveRef(pub f64, pub f64, pub DataRef);

scanner! { CurveRef,
	u0 u1 curv2d => CurveRef(u0, u1, curv2d)
}

/**
A reference to a surface, start and end parameters, and a curve.
*/
#[deriving(Decodable, Encodable, PartialEq, Show)]
pub struct SurfaceRef {
	pub surf: DataRef,
	pub q0: f64,
	pub q1: f64,
	pub curv2d: DataRef,
}

scanner! { SurfaceRef,
	surf q0 q1 curv2d => SurfaceRef {
		surf: surf,
		q0: q0,
		q1: q1,
		curv2d: curv2d,
	}
}

/**
Approximation to be used for drawing a curve.
*/
#[deriving(Decodable, Encodable, PartialEq, Show)]
pub enum CurveApprox {
	CurveConstParam(uint),
	CurveConstSpace(f64),
	CurveCurvatureDep(f64, f64),
}

scanner! { CurveApprox,
	"cparm" res => CurveConstParam(res),
	"cspace" maxlength => CurveConstSpace(maxlength),
	"curv" maxdist maxangle => CurveCurvatureDep(maxdist, maxangle),
}

/**
Approximation to be used for drawing a surface.
*/
#[deriving(Decodable, Encodable, PartialEq, Show)]
pub enum SurfaceApprox {
	SurfConstParam(uint, uint),
	SurfConstSpace(f64),
	SurfCurvatureDep(f64, f64),
}

scanner! { SurfaceApprox,
	"cparma" ures vres => SurfConstParam(ures, vres),
	"cparmb" uvres => SurfConstParam(uvres, uvres),
	"cspace" maxlength => SurfConstSpace(maxlength),
	"curv" maxdist maxangle => SurfCurvatureDep(maxdist, maxangle),
}

const NEWLINES: &'static [char] = &['\r', '\n'];

/**
Read a single line from a `Reader`.  This function will strip comments, paste continued lines together, and strip the trailing newline.
*/
pub fn read_obj_line<R: Reader>(r: &mut R) -> IoResult<String> {
	// Read in a complete line, doing line pasting as necessary.
	let mut accum = String::new();
	loop {
		let mut line = try!(::scan_util::io::read_line(r));
		{
			let line_nonl_len = line.as_slice().trim_right_chars(NEWLINES).len();
			line.truncate(line_nonl_len);
		}
		if line.as_slice().ends_with("\\") {
			accum.push_str(line.as_slice());
		} else {
			accum = line;
			break;
		}
	}

	// Strip comments
	if let Some(end) = accum.as_slice().find('#') {
		accum.truncate(end);
	}

	Ok(accum)
}

/**
Scan a single line from an OBJ file.  This function assumes that comments have been removed, continued lines pasted together and the line ending removed.

It will return an `Ok(None)` if the line is empty.  If the statement is not recognised, it will return an `Err`.
*/
pub fn scan_obj_line(s: &str) -> ScanResult<Option<ObjStatement>> {
	scan! { s,
		"v" x y z w? => Some(ObjVertex(x, y, z, w.unwrap_or(1.0))),
		"vp" u v w? => Some(ObjVertexParam(u, v, w.unwrap_or(1.0))),
		"vn" i j k => Some(ObjVertexNormal(i, j, k)),
		"vt" u v w? => Some(ObjVertexTexture(u, v, w.unwrap_or(0.0))),

		"cstype" rat cstype => Some(ObjCSType(rat, cstype)),
		"deg" degu degv => Some(ObjDegree(degu, degv)),
		"bmat" dir [mat]+ => Some(ObjBasisMatrix(dir, mat)),
		"step" stepu stepv? => Some(ObjStepSize(stepu, stepv)),

		"p" [vs]+ => Some(ObjPoints(vs)),
		"l" [vs]+ => Some(ObjLine(vs)),
		"f" [vs]+ => Some(ObjFace(vs)),
		"curv" u0 u1 [vs]{2,} => Some(ObjCurve(u0, u1, vs)),
		"curv2" [vps]{2,} => Some(Obj2dCurve(vps)),
		"surf" s0 s1 t0 t1 [vs]+ => Some(ObjSurface(s0, s1, t0, t1, vs)),

		"param" dir [ps]{2,} => Some(ObjParam(dir, ps)),
		"trim" [crs]+ => Some(ObjTrim(crs)),
		"hole" [crs]+ => Some(ObjHole(crs)),
		"scrv" [crs]+ => Some(ObjSpecialCurve(crs)),
		"sp" [vps]+ => Some(ObjSpecialPoint(vps)),
		"end" => Some(ObjEnd),

		"con" surf1 surf2 => Some(ObjConnect(surf1, surf2)),
		#[tokenizer="SpaceDelimited"]
		"g" [gs]+ => Some(ObjGroups(gs)),
		"s" (sg|"off") => Some(ObjSmoothingGroup(sg.unwrap_or(0))),
		"mg" (("0" | "off") res | sg res) => Some(ObjMergingGroup(sg.unwrap_or(0), res)),
		"o", ..name => Some(ObjObjectName(name.trim_left().into_string())),

		"bevel" on:OnOff => Some(ObjBevelInterp(on.to_bool())),
		"c_interp" on:OnOff => Some(ObjColorInterp(on.to_bool())),
		"d_interp" on:OnOff => Some(ObjDissolveInterp(on.to_bool())),
		"lod" level => Some(ObjLOD(level)),
		#[tokenizer="SpaceDelimited"]
		"maplib" [files]+ => Some(ObjTexMapLibraries(files)),
		#[tokenizer="SpaceDelimited"]
		"usemap" ("off"|name) => Some(ObjTexMap(name)),
		#[tokenizer="SpaceDelimited"]
		"usemtl" name => Some(ObjMtl(name)),
		#[tokenizer="SpaceDelimited"]
		"mtllib" [files]+ => Some(ObjMtlLibraries(files)),
		#[tokenizer="SpaceDelimited"]
		"shadow_obj" file => Some(ObjShadowCaster(file)),
		#[tokenizer="SpaceDelimited"]
		"trace_obj" file => Some(ObjReflectionObj(file)),
		"ctech" tech => Some(ObjCurveApprox(tech)),
		"stech" tech => Some(ObjSurfaceApprox(tech)),

		_ignore:() => None
	}
}

/*
This is a little helper to pase "on" and "off" as bools.
*/
struct OnOff(bool);

impl OnOff {
	pub fn to_bool(self) -> bool {
		let OnOff(v) = self;
		v
	}
}

scanner! { OnOff, "on" => OnOff(true), "off" => OnOff(false) }
