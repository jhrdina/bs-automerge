type replicaId = string;
type timestamp = (int, replicaId);
let encodeTimestamp: ((int, string)) => string;
module Timestamp: {
  type t = timestamp;
  let compare: ('a, 'a) => int;
};
module TimestampMap: {
  type key = Timestamp.t;
  type t('a) = Map.Make(Timestamp).t('a);
  let empty: t('a);
  let is_empty: t('a) => bool;
  let mem: (key, t('a)) => bool;
  let add: (key, 'a, t('a)) => t('a);
  let singleton: (key, 'a) => t('a);
  let remove: (key, t('a)) => t('a);
  let merge:
    ((key, option('a), option('b)) => option('c), t('a), t('b)) => t('c);
  let compare: (('a, 'a) => int, t('a), t('a)) => int;
  let equal: (('a, 'a) => bool, t('a), t('a)) => bool;
  let iter: ((key, 'a) => unit, t('a)) => unit;
  let fold: ((key, 'a, 'b) => 'b, t('a), 'b) => 'b;
  let for_all: ((key, 'a) => bool, t('a)) => bool;
  let exists: ((key, 'a) => bool, t('a)) => bool;
  let filter: ((key, 'a) => bool, t('a)) => t('a);
  let partition: ((key, 'a) => bool, t('a)) => (t('a), t('a));
  let cardinal: t('a) => int;
  let bindings: t('a) => list((key, 'a));
  let min_binding: t('a) => (key, 'a);
  let max_binding: t('a) => (key, 'a);
  let choose: t('a) => (key, 'a);
  let split: (key, t('a)) => (t('a), option('a), t('a));
  let find: (key, t('a)) => 'a;
  let map: ('a => 'b, t('a)) => t('b);
  let mapi: ((key, 'a) => 'b, t('a)) => t('b);
};
type value =
  | String(string)
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Null
  | EmptyObject
  | EmptyArray;
let encodeValue: value => string;
type mut =
  | Assign(value)
  | Insert(value)
  | Delete;
let encodeMut: mut => string;
type cursorItem =
  | Doc
  | Str(string)
  | Head
  | Tail
  | Timestamp(timestamp);
module CursorItem: {
  type t = cursorItem;
  let compare: (cursorItem, cursorItem) => int;
};
module CursorItemMap: {
  type key = CursorItem.t;
  type t('a) = Map.Make(CursorItem).t('a);
  let empty: t('a);
  let is_empty: t('a) => bool;
  let mem: (key, t('a)) => bool;
  let add: (key, 'a, t('a)) => t('a);
  let singleton: (key, 'a) => t('a);
  let remove: (key, t('a)) => t('a);
  let merge:
    ((key, option('a), option('b)) => option('c), t('a), t('b)) => t('c);
  let compare: (('a, 'a) => int, t('a), t('a)) => int;
  let equal: (('a, 'a) => bool, t('a), t('a)) => bool;
  let iter: ((key, 'a) => unit, t('a)) => unit;
  let fold: ((key, 'a, 'b) => 'b, t('a), 'b) => 'b;
  let for_all: ((key, 'a) => bool, t('a)) => bool;
  let exists: ((key, 'a) => bool, t('a)) => bool;
  let filter: ((key, 'a) => bool, t('a)) => t('a);
  let partition: ((key, 'a) => bool, t('a)) => (t('a), t('a));
  let cardinal: t('a) => int;
  let bindings: t('a) => list((key, 'a));
  let min_binding: t('a) => (key, 'a);
  let max_binding: t('a) => (key, 'a);
  let choose: t('a) => (key, 'a);
  let split: (key, t('a)) => (t('a), option('a), t('a));
  let find: (key, t('a)) => 'a;
  let map: ('a => 'b, t('a)) => t('b);
  let mapi: ((key, 'a) => 'b, t('a)) => t('b);
};
type cursor;
let encodeCursor: cursor => string;
type state;
let encodeState: state => Json.t;
let doc: cursor;
let get: (cursor, string) => cursor;
let idx: (cursor, state, int) => cursor;
let keys: (cursor, state) => list(CursorItemMap.key);
let val_: (cursor, state) => TimestampMap.t(value);
let makeOp: (state, cursor, mut) => state;
let makeAssign: (cursor, state, value) => state;
let makeInsert: (cursor, state, value) => state;
let makeDelete: (cursor, state) => state;
let make: replicaId => state;

type opSet;
let recv: (state, opSet) => state;
let send: state => opSet;