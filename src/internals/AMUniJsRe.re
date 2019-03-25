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

  module ConflictValues = {
    type json = t;
    type t = AMJsRe.conflict;
    let fold = (f, t, acc) =>
      Array.fold_left(
        (acc, actorIdStr) =>
          f(
            ActorId.ofString(actorIdStr),
            t->Js.Dict.unsafeGet(actorIdStr),
            acc,
          ),
        acc,
        t->Js.Dict.keys,
      );

    let get = (actorId, t) => Js.Dict.get(t, actorId);
  };

  type conflictable = {
    value: t,
    conflicts: option(ConflictValues.t),
  };

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

    let getC = (i, t) =>
      get(i, t)
      |?>> (
        value => {
          {value, conflicts: t |> AMJsRe.getArrayConflicts |> get(i)};
        }
      );

    let foldLeft = (f, acc, t) => Array.fold_left(f, acc, t);

    let foldLeftC = (f, acc, t) =>
      Js.Array.reducei(
        (acc, value, i) =>
          f(
            acc,
            {value, conflicts: t |> AMJsRe.getArrayConflicts |> get(i)},
          ),
        acc,
        t,
      );

    let foldRight = (f, t, acc) => Array.fold_right(f, t, acc);

    let foldRightC = (f, t, acc) =>
      Js.Array.reduceRighti(
        (acc, value, i) =>
          f(
            {value, conflicts: t |> AMJsRe.getArrayConflicts |> get(i)},
            acc,
          ),
        acc,
        t,
      );

    let filter = (f, t) => {
      let index = ref(Array.length(t) - 1);
      while (index^ >= 0) {
        if (!f(Js.Array.unsafe_get(t, index^))) {
          Js.Array.spliceInPlace(~pos=index^, ~remove=1, t) |> ignore;
        };
        index := index^ - 1;
      };
      t;
    };
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

    let getC = (key, t) =>
      get(key, t)
      |?>> (
        value => {
          value,
          conflicts: t |> AMJsRe.getObjectConflicts |> get(key),
        }
      );

    let remove: (string, t) => t = [%bs.raw
      (key, t) => "{
        if (t[key] !== undefined) {
          delete t[key];
        }
        return t;
      }"
    ];

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

    let foldC = (f, t, acc) =>
      t
      |> Js.Dict.keys
      |> Array.fold_left(
           (acc, key) =>
             f(
               key,
               {
                 value: Js.Dict.unsafeGet(t, key),
                 conflicts: t |> AMJsRe.getObjectConflicts |> get(key),
               },
               acc,
             ),
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