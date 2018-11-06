open InfixRe;

module ChangeSet = AMJsRe.ChangeSet;

module Clock = AMJsRe.Clock;

module ActorId = {
  type t = string;
  let ofString = str => str;
  let toString = t => t;
};

type t = AMJsRe.t;
type transaction = AMJsRe.t;

module Json = {
  type t = Js.Json.t;
  type nonrec transaction = transaction;

  let string = Js.Json.string;
  let asString = Js.Json.decodeString;
  let int = i => i |> float_of_int |> Js.Json.number;
  let asInt = t => t |> Js.Json.decodeNumber |?>> int_of_float;
  let float = Js.Json.number;
  let asFloat = Js.Json.decodeNumber;
  let bool = Js.Json.boolean;
  let asBool = Js.Json.decodeBoolean;

  module List = {
    type json = t;
    type nonrec t = array(t);
    type nonrec transaction = transaction;
    let create = () => [||];
    let ofJson = Js.Json.decodeArray;
    let toJson = Js.Json.array;
    let set = (i, value, t) => {
      t[i] = value;
      t;
    };

    let prepend = (value, t) => {
      Js.Array.unshift(value, t) |> ignore;
      t;
    };

    let get = (i, t) =>
      switch (t[i]) {
      | value => Some(value)
      | exception (Invalid_argument(_)) => None
      };

    let foldLeft = (f, acc, t) => Array.fold_left(f, acc, t);

    let foldRight = (f, t, acc) => Array.fold_right(f, t, acc);
  };

  module Map = {
    type json = t;
    type nonrec t = Js.Dict.t(json);
    type nonrec transaction = transaction;
    let create = () => Js.Dict.empty();
    let ofJson = Js.Json.decodeObject;
    let toJson = Js.Json.object_;

    let add = (key, value, t) => {
      Js.Dict.set(t, key, value);
      t;
    };

    let get = (key, t) => Js.Dict.get(t, key);

    let fold = (f, t, acc) =>
      t
      |> Js.Dict.keys
      |> Array.fold_left(
           (acc, key) =>
             switch (Js.Dict.get(t, key)) {
             | Some(value) => f(key, value, acc)
             | None => acc
             },
           acc,
         );
  };
};

let make = actorId => AMJsRe.initWithActorId(actorId);

let change = (message, f, t) =>
  AMJsRe.change(t, message, doc => f(doc) |> ignore);

let root = t => t;
let getClock = AMJsRe.getClock;
let getChangesFromTime = (clock, t) => AMJsRe.getMissingChanges(t, clock);
let applyChanges = (changes, t) => AMJsRe.applyChanges(t, changes);

let save = t =>
  Js.Json.object_(
    Js.Dict.fromList([
      ("actorId", Js.Json.string(AMJsRe.getActorId(t))),
      ("state", AMJsRe.save(t)),
    ]),
  )
  |> Js.Json.stringify;

let load = str =>
  switch (str |> Js.Json.parseExn) {
  | json =>
    switch (json |> Js.Json.decodeObject) {
    | Some(record) =>
      switch (
        Js.Dict.get(record, "state"),
        Js.Dict.get(record, "actorId") |?> Js.Json.decodeString,
      ) {
      | (Some(state), Some(actorId)) => AMJsRe.load(state, actorId)
      | _ => None
      }
    | None => None
    }
  | exception _ => None
  };