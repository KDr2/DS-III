struct Monad;

trait Monad_<T1, T2, A> {
    fn ret(a: A) -> T1;
    fn bind(a: T1, f: fn(A)->T2) ->T2;
}

#[derive(Debug)]
enum Maybe<T> {
    Nothing,
    Just(T),
}

impl<A, B> Monad_<Maybe<A>, Maybe<B>, A> for Monad{
    fn ret(a: A) -> Maybe<A>{ Maybe::Just(a) }
    fn bind(a: Maybe<A>, f: fn(A)->Maybe<B>) ->Maybe<B> {
        match a {
            Maybe::Nothing => Maybe::Nothing,
            Maybe::Just(v) => f(v),
        }
    }
}

fn main() {
    // let a = Maybe::Just(1);
    // or
    let a = <Monad as Monad_::<Maybe<i32>, Maybe<()>, i32>>::ret(1);
    let b = Monad::bind(a, |x| {
        let s = "=".repeat((x as usize) * 3);
        Maybe::Just(s)
    });
    let c = Maybe::Nothing;
    let d = Monad::bind(c, |x:i32| Maybe::Just(x+1));
    println!("Hi, {:?}, {:?}!", b, d);
}
