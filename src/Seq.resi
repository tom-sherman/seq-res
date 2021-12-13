/*
  A sequence [xs] of type ['a t] is a delayed list of elements of type ['a]. Such a sequence is queried by performing a
  function application [xs()]. This function application returns a node, allowing the caller to determine whether the
  sequence is empty or nonempty, and in the latter case, to obtain its head and tail.
*/
type rec t<'a> = unit => node<'a>

/*
  A node is either [Nil], which means that the sequence is empty, or [Cons(x, xs)], which means that [x] is the first
  element of the sequence and that [xs] is the remainder of the sequence.
*/
and node<+'a> =
  | Nil
  | Cons('a, t<'a>)

let isEmpty: t<'a> => bool

let uncons: t<'a> => option<('a, t<'a>)>

let length: t<'a> => int

let forEach: (t<'a>, 'a => unit) => unit

let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

let forEachi: (t<'a>, ('a, int) => unit) => unit

let reduceLefti: (t<'a>, 'b, ('b, 'a, int) => 'b) => 'b

let every: (t<'a>, 'a => bool) => bool

let some: (t<'a>, 'a => bool) => bool

let find: (t<'a>, 'a => bool) => option<'a>

let findMap: (t<'a>, 'a => option<'b>) => option<'b>

let equal: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

let compare: (t<'a>, t<'b>, ('a, 'b) => int) => int

let empty: t<'a>

let return: 'a => t<'a>

let cons: (t<'a>, 'a) => t<'a>
let prepend: (t<'a>, 'a) => t<'a>

let init: (int, int => 'a) => t<'a>

let unfold: ('b, 'b => option<('a, 'b)>) => t<'a>

let repeat: 'a => t<'a>

let forever: (unit => 'a) => t<'a>

let cycle: t<'a> => t<'a>

let iterate: ('a, 'a => 'a) => t<'a>

let map: (t<'a>, 'a => 'b) => t<'b>

let mapi: (t<'a>, ('a, int) => 'b) => t<'b>

let filter: (t<'a>, 'a => bool) => t<'a>

let filterMap: (t<'a>, 'a => option<'b>) => t<'b>

let scan: (t<'a>, 'b, ('b, 'a) => 'b) => t<'b>

let take: (t<'a>, int) => t<'a>

let drop: (t<'a>, int) => t<'a>

let takeWhile: (t<'a>, 'a => bool) => t<'a>

let dropWhile: (t<'a>, 'a => bool) => t<'a>

let group: (t<'a>, ('a, 'a) => bool) => t<t<'a>>

let memoize: t<'a> => t<'a>

exception Forced_twice

let once: t<'a> => t<'a>

let transpose: t<t<'a>> => t<t<'a>>

let append: (t<'a>, t<'a>) => t<'a>

let concat: t<t<'a>> => t<'a>

let flatMap: (t<'a>, 'a => t<'b>) => t<'b>

let concatMap: (t<'a>, 'a => t<'b>) => t<'b>

let zip: (t<'a>, t<'b>) => t<('a, 'b)>

let map2: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

let interleave: (t<'a>, t<'a>) => t<'a>

let sortedMerge: (t<'a>, t<'a>, ('a, 'a) => int) => t<'a>

let product: (t<'a>, t<'b>) => t<('a, 'b)>

let mapProduct: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

let unzip: t<('a, 'b)> => (t<'a>, t<'b>)

let split: t<('a, 'b)> => (t<'a>, t<'b>)

let partitionMap: (t<'a>, 'a => result<'b, 'c>) => (t<'b>, t<'c>)

let partition: (t<'a>, 'a => bool) => (t<'a>, t<'a>)

let ofDispenser: (unit => option<'a>) => t<'a>

let toDispenser: (t<'a>, unit) => option<'a>

let ints: int => t<int>

let get: (t<'a>, int) => option<'a>

let fromList: list<'a> => t<'a>

let toList: t<'a> => list<'a>

let fromArray: array<'a> => t<'a>

let toArray: t<'a> => array<'a>

let reverse: t<'a> => t<'a>
