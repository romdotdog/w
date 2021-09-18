pub enum Atom {
	BinOp(Box<Atom>, Op, Box<Atom>),
	
}