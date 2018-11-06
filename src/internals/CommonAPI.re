module type ActorId = {
  type t;
  let ofString: string => t;
  let toString: t => string;
};

/* module type ConflictMap = {
     type t;
     type json;
     let fold: ((ActorId.t, json, 'a) => 'a, t, 'a) => 'a;
     let get: (ActorId.t, t) => option(json);
   }; */

module type JsonList = {
  type json;
  type t;

  let create: unit => t;
  let ofJson: json => option(t);
  let toJson: t => json;
  let set: (int, json, t) => t;
  /** [set(index, value, listJson, transaction)] checks that [listJson]
          really is a list and sets its element at position [index] to [value].
          */
  let prepend: (json, t) => t;
  let get: (int, t) => option(json);
  let foldLeft: (('a, json) => 'a, 'a, t) => 'a;
  let foldRight: ((json, 'a) => 'a, t, 'a) => 'a;
};

module type JsonMap = {
  type json;
  type t;

  let create: unit => t;
  let ofJson: json => option(t);
  let toJson: t => json;
  let add: (string, json, t) => t;
  /** [add(key, value, mapJson, transaction)] checks that [mapJson] really
          is a map and sets its element at [key] to [value]. */
  let get: (string, t) => option(json);
  /* let getWithConflicts: (string, t) => option(conflictable); */
  let fold: ((string, json, 'a) => 'a, t, 'a) => 'a;
};

module type Json = {
  /* Basically a ctx */
  type t;
  /* type conflictable =
     | SingleValue(t)
     | WithConflicts(t); */

  module List: JsonList with type json = t;
  module Map: JsonMap with type json = t;

  let string: string => t;
  let asString: t => option(string);
  let int: int => t;
  let asInt: t => option(int);
  let float: float => t;
  let asFloat: t => option(float);
  let bool: bool => t;
  let asBool: t => option(bool);
};

module type ChangeSet = {
  type t;
  let toString: t => string;
  let fromString: string => option(t);
};

module type Clock = {
  type t;
  let lessOrEqual: (t, t) => bool;
  let toString: t => string;
  let fromString: string => option(t);
};

module type CommonAPI = {
  type transaction;

  module ChangeSet: ChangeSet;
  module Clock: Clock;
  module ActorId: ActorId;
  module Json: Json;

  type t;

  let make: ActorId.t => t;
  let change: (string, Json.Map.t => Json.Map.t, t) => t;
  let root: t => Json.Map.t;

  let getClock: t => Clock.t;
  let getChangesFromTime: (Clock.t, t) => ChangeSet.t;
  let applyChanges: (ChangeSet.t, t) => t;

  let save: t => string;
  let load: string => option(t);
};