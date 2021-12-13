type rec node<+'a> =
  | Nil
  | Cons('a, t<'a>)

and t<'a> = unit => node<'a>

let empty = () => Nil

let return = (x): t<'a> => () => Cons(x, empty)

let cons = (xs, x): t<'a> => () => Cons(x, xs)
let prepend = cons

let rec append = (seq1, seq2, ()) =>
  switch seq1() {
  | Nil => seq2()
  | Cons(x, next) => Cons(x, append(next, seq2))
  }

let rec map = (seq, f, ()) =>
  switch seq() {
  | Nil => Nil
  | Cons(x, next) => Cons(f(x), map(next, f))
  }

let rec filterMap = (seq, f, ()) =>
  switch seq() {
  | Nil => Nil
  | Cons(x, next) =>
    switch f(x) {
    | None => filterMap(next, f, ())
    | Some(y) => Cons(y, filterMap(next, f))
    }
  }

let rec filter = (seq, f, ()) =>
  switch seq() {
  | Nil => Nil
  | Cons(x, next) =>
    if f(x) {
      Cons(x, filter(next, f))
    } else {
      filter(next, f, ())
    }
  }

let rec concat = (seq, ()) =>
  switch seq() {
  | Nil => Nil
  | Cons(x, next) => append(x, concat(next), ())
  }

let rec flatMap = (seq, f, ()) =>
  switch seq() {
  | Nil => Nil
  | Cons(x, next) => append(f(x), flatMap(next, f), ())
  }

let concatMap = flatMap

let rec reduce = (seq, acc, f) =>
  switch seq() {
  | Nil => acc
  | Cons(x, next) =>
    let acc = f(acc, x)
    reduce(next, acc, f)
  }

let rec forEach = (seq, f) =>
  switch seq() {
  | Nil => ()
  | Cons(x, next) =>
    f(x)
    forEach(next, f)
  }

let rec unfold = (u, f, ()) =>
  switch f(u) {
  | None => Nil
  | Some(x, u') => Cons(x, unfold(u', f))
  }

let isEmpty = xs =>
  switch xs() {
  | Nil => true
  | Cons(_, _) => false
  }

let uncons = xs =>
  switch xs() {
  | Cons(x, xs) => Some(x, xs)
  | Nil => None
  }

let rec lengthAux = (xs, accu) =>
  switch xs() {
  | Nil => accu
  | Cons(_, xs) => lengthAux(xs, accu + 1)
  }

@inline let length = xs => lengthAux(xs, 0)

let rec iteriAux = (xs, i, f) =>
  switch xs() {
  | Nil => ()
  | Cons(x, xs) =>
    f(x, i)
    iteriAux(xs, i + 1, f)
  }

@inline let forEachi = (xs, f) => iteriAux(xs, 0, f)

let rec foldLeftiAux = (xs, accu, i, f) =>
  switch xs() {
  | Nil => accu
  | Cons(x, xs) =>
    let accu = f(accu, x, i)
    foldLeftiAux(xs, accu, i + 1, f)
  }

@inline let reduceLefti = (xs, accu, f) => foldLeftiAux(xs, accu, 0, f)

let rec every = (xs, p) =>
  switch xs() {
  | Nil => true
  | Cons(x, xs) => p(x) && every(xs, p)
  }

let rec some = (xs, p) =>
  switch xs() {
  | Nil => false
  | Cons(x, xs) => p(x) || some(xs, p)
  }

let rec find = (xs, p) =>
  switch xs() {
  | Nil => None
  | Cons(x, xs) =>
    if p(x) {
      Some(x)
    } else {
      find(xs, p)
    }
  }

let rec findMap = (xs, f) =>
  switch xs() {
  | Nil => None
  | Cons(x, xs) =>
    switch f(x) {
    | None => findMap(xs, f)
    | Some(_) as result => result
    }
  }

let rec equal = (xs, ys, eq) =>
  switch (xs(), ys()) {
  | (Nil, Nil) => true
  | (Cons(x, xs), Cons(y, ys)) => eq(x, y) && equal(xs, ys, eq)
  | (Nil, Cons(_, _))
  | (Cons(_, _), Nil) => false
  }

let rec compare = (xs, ys, cmp) =>
  switch (xs(), ys()) {
  | (Nil, Nil) => 0
  | (Cons(x, xs), Cons(y, ys)) =>
    let c = cmp(x, y)
    if c != 0 {
      c
    } else {
      compare(xs, ys, cmp)
    }
  | (Nil, Cons(_, _)) => -1
  | (Cons(_, _), Nil) => 1
  }

/* [init_aux f i j] is the sequence [f i, ..., f (j-1)]. */

let rec initAux = (f, i, j, ()) =>
  if i < j {
    Cons(f(i), initAux(f, i + 1, j))
  } else {
    Nil
  }

let init = (n, f) =>
  if n < 0 {
    invalid_arg("Seq.init")
  } else {
    initAux(f, 0, n)
  }

let rec repeat = (x, ()) => Cons(x, repeat(x))

let rec forever = (f, ()) => Cons(f(), forever(f))

/* This preliminary definition of [cycle] requires the sequence [xs]
   to be nonempty. Applying it to an empty sequence would produce a
   sequence that diverges when it is forced. */

let rec cycleNonEmpty = (xs, ()) => append(xs, cycleNonEmpty(xs), ())

/* [cycle xs] checks whether [xs] is empty and, if so, returns an empty
   sequence. Otherwise, [cycle xs] produces one copy of [xs] followed
   with the infinite sequence [cycle_nonempty xs]. Thus, the nonemptiness
   check is performed just once. */

let cycle = (xs, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs') => Cons(x, append(xs', cycleNonEmpty(xs)))
  }

/* [iterate1 f x] is the sequence [f x, f (f x), ...].
   It is equivalent to [tail (iterate f x)].
   [iterate1] is used as a building block in the definition of [iterate]. */

let rec iterate1 = (x, f, ()) => {
  let y = f(x)
  Cons(y, iterate1(y, f))
}

/* [iterate f x] is the sequence [x, f x, ...]. */

/* The reason why we give this slightly indirect definition of [iterate],
   as opposed to the more naive definition that may come to mind, is that
   we are careful to avoid evaluating [f x] until this function call is
   actually necessary. The naive definition (not shown here) computes the
   second argument of the sequence, [f x], when the first argument is
   requested by the user. */

let iterate = (x, f) => cons(iterate1(x, f), x)

let rec mapiAux = (xs, i, f, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) => Cons(f(x, i), mapiAux(xs, i + 1, f))
  }

@inline let mapi = (f, xs) => mapiAux(f, 0, xs)

/* [tail_scan f s xs] is equivalent to [tail (scan f s xs)].
 [tail_scan] is used as a building block in the definition of [scan]. */

/* This slightly indirect definition of [scan] is meant to avoid computing
 elements too early; see the above comment about [iterate1] and [iterate]. */

let rec tailScan = (xs, s, f, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) =>
    let s = f(s, x)
    Cons(s, tailScan(xs, s, f))
  }

let scan = (xs, s, f) => cons(tailScan(xs, s, f), s)

/* [take] is defined in such a way that [take 0 xs] returns [empty]
 immediately, without allocating any memory. */

let rec takeAux = (xs, n) =>
  if n == 0 {
    empty
  } else {
    () =>
      switch xs() {
      | Nil => Nil
      | Cons(x, xs) => Cons(x, takeAux(xs, n - 1))
      }
  }

let take = (xs, n) => {
  if n < 0 {
    invalid_arg("Seq.take")
  }
  takeAux(xs, n)
}

/* [force_drop n xs] is equivalent to [drop n xs ()].
   [force_drop n xs] requires [n > 0].
   [force_drop] is used as a building block in the definition of [drop]. */

let rec forceDrop = (xs, n) =>
  switch xs() {
  | Nil => Nil
  | Cons(_, xs) =>
    let n = n - 1
    if n == 0 {
      xs()
    } else {
      forceDrop(xs, n)
    }
  }

/* [drop] is defined in such a way that [drop 0 xs] returns [xs] immediately,
 without allocating any memory. */

let drop = (xs, n) =>
  if n < 0 {
    invalid_arg("Seq.drop")
  } else if n == 0 {
    xs
  } else {
    () => forceDrop(xs, n)
  }

let rec takeWhile = (xs, p, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) =>
    if p(x) {
      Cons(x, takeWhile(xs, p))
    } else {
      Nil
    }
  }

let rec dropWhile = (xs, p, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) as node =>
    if p(x) {
      dropWhile(xs, p, ())
    } else {
      node
    }
  }

let rec group = (xs: t<'a>, eq, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) => Cons(cons(takeWhile(xs, eq(x)), x), group(dropWhile(xs, eq(x)), eq))
  }

exception Forced_twice

module Suspension = {
  type suspension<'a> = unit => 'a

  /* Conversions. */

  let toLazy: suspension<'a> => Lazy.t<'a> = Lazy.from_fun
  /* fun s -> lazy (s()) */

  let fromLazy = (s: Lazy.t<'a>): suspension<'a> => () => Lazy.force(s)

  /* [memoize] turns an arbitrary suspension into a persistent suspension. */

  let memoize = (s: suspension<'a>): suspension<'a> => fromLazy(toLazy(s))

  /* [failure] is a suspension that fails when forced. */

  let failure: suspension<_> = () =>
    /* A suspension created by [once] has been forced twice. */
    raise(Forced_twice)

  /* If [f] is a suspension, then [once f] is a suspension that can be forced
     at most once. If it is forced more than once, then [Forced_twice] is
     raised. */

  let once = (f: suspension<'a>): suspension<'a> => {
    let action = ref(f)
    () => {
      /* Get the function currently stored in [action], and write the
         function [failure] in its place, so the next access will result
         in a call to [failure()]. */

      let cur = action.contents
      action.contents = failure
      cur()
    }
  }
} /* Suspension */

let rec memoize = xs =>
  Suspension.memoize(() =>
    switch xs() {
    | Nil => Nil
    | Cons(x, xs) => Cons(x, memoize(xs))
    }
  )

let rec once = xs =>
  Suspension.once(() =>
    switch xs() {
    | Nil => Nil
    | Cons(x, xs) => Cons(x, once(xs))
    }
  )

let rec zip = (xs, ys, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) =>
    switch ys() {
    | Nil => Nil
    | Cons(y, ys) => Cons((x, y), zip(xs, ys))
    }
  }

let rec map2 = (xs, ys, f, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) =>
    switch ys() {
    | Nil => Nil
    | Cons(y, ys) => Cons(f(x, y), map2(xs, ys, f))
    }
  }

let rec interleave = (xs, ys, ()) =>
  switch xs() {
  | Nil => ys()
  | Cons(x, xs) => Cons(x, interleave(ys, xs))
  }

/* [sorted_merge1l cmp x xs ys] is equivalent to
     [sorted_merge cmp (cons x xs) ys].

   [sorted_merge1r cmp xs y ys] is equivalent to
     [sorted_merge cmp xs (cons y ys)].

   [sorted_merge1 cmp x xs y ys] is equivalent to
     [sorted_merge cmp (cons x xs) (cons y ys)].

   These three functions are used as building blocks in the definition
   of [sorted_merge]. */

let rec sortedMerge1L = (cmp, x, xs, ys, ()) =>
  switch ys() {
  | Nil => Cons(x, xs)
  | Cons(y, ys) => sortedMerge1(cmp, x, xs, y, ys)
  }

and sortedMerge1R = (cmp, xs, y, ys, ()) =>
  switch xs() {
  | Nil => Cons(y, ys)
  | Cons(x, xs) => sortedMerge1(cmp, x, xs, y, ys)
  }

and sortedMerge1 = (cmp, x, xs, y, ys) =>
  if cmp(x, y) <= 0 {
    Cons(x, sortedMerge1R(cmp, xs, y, ys))
  } else {
    Cons(y, sortedMerge1L(cmp, x, xs, ys))
  }

let sortedMerge = (xs: t<'a>, ys: t<'a>, cmp): t<'a> =>
  () =>
    switch (xs(), ys()) {
    | (Nil, Nil) => Nil
    | (Nil, c)
    | (c, Nil) => c
    | (Cons(x, xs), Cons(y, ys)) => sortedMerge1(cmp, x, xs, y, ys)
    }

let rec mapFst = (xys, ()) =>
  switch xys() {
  | Nil => Nil
  | Cons((x, _), xys) => Cons(x, mapFst(xys))
  }

let rec mapSnd = (xys, ()) =>
  switch xys() {
  | Nil => Nil
  | Cons((_, y), xys) => Cons(y, mapSnd(xys))
  }

let unzip = xys => (mapFst(xys), mapSnd(xys))

let split = unzip

/* [filter_map_find_left_map f xs] is equivalent to
 [filter_map Either.find_left (map f xs)]. */

let rec filterMapFindOkMap = (xs, f, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) =>
    switch f(x) {
    | Ok(y) => Cons(y, filterMapFindOkMap(xs, f))
    | Error(_) => filterMapFindOkMap(xs, f, ())
    }
  }

let rec filterMapFindErrorMap = (xs, f, ()) =>
  switch xs() {
  | Nil => Nil
  | Cons(x, xs) =>
    switch f(x) {
    | Ok(_) => filterMapFindErrorMap(xs, f, ())
    | Error(z) => Cons(z, filterMapFindErrorMap(xs, f))
    }
  }

let partitionMap = (xs, f) => (filterMapFindOkMap(xs, f), filterMapFindErrorMap(xs, f))

let partition = (xs, p) => (filter(xs, p), filter(xs, x => !p(x)))

/* If [xss] is a matrix (a sequence of rows), then [peel xss] is a pair of
   the first column (a sequence of elements) and of the remainder of the
   matrix (a sequence of shorter rows). These two sequences have the same
   length. The rows of the matrix [xss] are not required to have the same
   length. An empty row is ignored. */

/* Because [peel] uses [unzip], its argument must be persistent. The same
 remark applies to [transpose], [diagonals], [product], etc. */

let peel = (xss: t<t<'a>>) => unzip(filterMap(xss, uncons))

let rec transpose = (xss, ()) => {
  let (heads, tails) = peel(xss)
  if isEmpty(heads) {
    assert isEmpty(tails)
    Nil
  } else {
    Cons(heads, transpose(tails))
  }
}

/* The internal function [diagonals] takes an extra argument, [remainders],
   which contains the remainders of the rows that have already been
   discovered. */

let rec diagonals = (remainders, xss, ()) =>
  switch xss() {
  | Cons(xs, xss) =>
    switch xs() {
    | Cons(x, xs) =>
      /* We discover a new nonempty row [x :: xs]. Thus, the next diagonal
             is [x :: heads]: this diagonal begins with [x] and continues with
             the first element of every row in [remainders]. In the recursive
             call, the argument [remainders] is instantiated with [xs ::
             tails], which means that we have one more remaining row, [xs],
             and that we keep the tails of the pre-existing remaining rows. */
      let (heads, tails) = peel(remainders)
      Cons(cons(heads, x), diagonals(cons(tails, xs), xss))
    | Nil =>
      /* We discover a new empty row. In this case, the new diagonal is
             just [heads], and [remainders] is instantiated with just [tails],
             as we do not have one more remaining row. */
      let (heads, tails) = peel(remainders)
      Cons(heads, diagonals(tails, xss))
    }
  | Nil =>
    /* There are no more rows to be discovered. There remains to exhaust
     the remaining rows. */
    transpose(remainders, ())
  }

/* If [xss] is a matrix (a sequence of rows), then [diagonals xss] is
   the sequence of its diagonals.

   The first diagonal contains just the first element of the
   first row. The second diagonal contains the first element of the
   second row and the second element of the first row; and so on.
   This kind of diagonal is in fact sometimes known as an antidiagonal.

   - Every diagonal is a finite sequence.
   - The rows of the matrix [xss] are not required to have the same length.
   - The matrix [xss] is not required to be finite (in either direction).
   - The matrix [xss] must be persistent. */

let diagonals = xss => diagonals(empty, xss)

let mapProduct = (xs, ys, f) => concat(diagonals(map(xs, x => map(ys, y => f(x, y)))))

let product = (xs, ys): t<'a> => mapProduct(xs, ys, (x, y) => (x, y))

let ofDispenser = it => {
  let rec c = () =>
    switch it() {
    | None => Nil
    | Some(x) => Cons(x, c)
    }

  c
}

let toDispenser = xs => {
  let s = ref(xs)
  () =>
    switch s.contents() {
    | Nil => None
    | Cons(x, xs) =>
      s := xs
      Some(x)
    }
}

let rec ints = (i, ()) => Cons(i, ints(i + 1))

// let slice = (xs, i, k) => xs->drop(i)->take(k - i + 1)

let reverse = xs => {
  let rec aux = (acc, xs) =>
    switch uncons(xs) {
    | None => acc
    | Some(h, t) => aux(cons(acc, h), t)
    }
  aux(empty, xs)
}

let toArray = xs => {
  let arr = []
  xs->forEach(x => arr->Js.Array2.push(x)->ignore)
  arr
}

let fromArray = arr => unfold(0, i => arr->Belt.Array.get(i)->Belt.Option.map(x => (x, i + 1)))

let rec toList = xs =>
  switch xs() {
  | Nil => list{}
  | Cons(x, xs) => list{x, ...toList(xs)}
  }

let rec fromList = l =>
  switch l {
  | list{} => () => Nil
  | list{head, ...tail} => () => Cons(head, fromList(tail))
  }

let get = (xs, i) =>
  switch xs->drop(i)->uncons {
  | None => None
  | Some(x, _) => Some(x)
  }
