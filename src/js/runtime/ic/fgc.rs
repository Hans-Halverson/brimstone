/// FGC: Fast Guarded Computation
///
/// A monadic type that's used by executors for IC Stubs
pub enum FGC<Arg, BailReason> {
    Ok(Arg),
    Bail(BailReason),
}

impl<Arg, BailReason> FGC<Arg, BailReason> {
    #[inline]
    pub fn new(arg: Arg) -> FGC<Arg, BailReason> {
        Self::Ok(arg)
    }

    /// Check something about the inner `val`
    /// returning a new BailReason if the check fails
    #[inline(always)]
    pub fn check<F>(self, f: F, b: BailReason) -> FGC<Arg, BailReason>
    where
        F: FnOnce(&Arg) -> bool,
    {
        match self {
            FGC::Ok(ref v) => {
                if !f(v) {
                    Self::Bail(b)
                } else {
                    self
                }
            }
            FGC::Bail(_) => self,
        }
    }

    /// Apply some operation to the inner value
    /// of FBC - this only bails if the inner value
    /// of the FBC monad is already the Bail variant
    #[inline(always)]
    pub fn map<F, NewArg>(self, f: F) -> FGC<NewArg, BailReason>
    where
        F: FnOnce(Arg) -> NewArg,
    {
        match self {
            FGC::Ok(v) => FGC::Ok(f(v)),
            FGC::Bail(br) => FGC::Bail(br),
        }
    }
}
