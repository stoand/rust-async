use std::convert::From;
use std::boxed::Box;

pub type Future<T> = Box<Fn(&Fn(T) -> ())>;

// TODO
// impl From<T> for Future<T> {
//    fn from(t: T) -> Self {
//        Box::new(move |callback: &Fn(T)| callback(t))
//    }
// }
