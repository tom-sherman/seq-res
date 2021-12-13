type iteratorResult<'a> = {done: bool, value: 'a}
type t<'a> = {next: unit => iteratorResult<'a>}

@module("../util/InternalIterator.js") external fromSeq: Seq.t<'a> => t<'a> = "fromSeq"
