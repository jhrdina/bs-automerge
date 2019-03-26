module type Stringifiable = {
  type t;
  let toString: t => string;
  let ofString: string => option(t);
};

module type CommonAPI = {
  type actorId;

  type transaction;

  module ChangeSet: {
    type t;
    let toString: t => string;
    let fromString: string => option(t);
  };

  module Clock: {
    type t;
    let lessOrEqual: (t, t) => bool;
    let toString: t => string;
    let fromString: string => option(t);
  };

  module Json: {
    /* Basically a ctx */
    type t;

    module ConflictValues: {
      type json = t;
      type t;

      let fold: ((actorId, json, 'a) => 'a, t, 'a) => 'a;
      let get: (actorId, t) => option(json);
    };

    type conflictable = {
      value: t,
      conflicts: option(ConflictValues.t),
    };

    module List: {
      type json = t;
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
      let getC: (int, t) => option(conflictable);
      let foldLeft: (('a, json) => 'a, 'a, t) => 'a;
      let foldLeftC: (('a, conflictable) => 'a, 'a, t) => 'a;
      let foldRight: ((json, 'a) => 'a, t, 'a) => 'a;
      let foldRightC: ((conflictable, 'a) => 'a, t, 'a) => 'a;
      let filter: (json => bool, t) => t;
    };

    module Map: {
      type json = t;
      type t;

      let create: unit => t;
      let ofJson: json => option(t);
      let toJson: t => json;
      let add: (string, json, t) => t;
      /** [add(key, value, mapJson, transaction)] checks that [mapJson] really
          is a map and sets its element at [key] to [value]. */
      let remove: (string, t) => t;
      let get: (string, t) => option(json);
      let getC: (string, t) => option(conflictable);
      let fold: ((string, json, 'a) => 'a, t, 'a) => 'a;
      let foldC: ((string, conflictable, 'a) => 'a, t, 'a) => 'a;
    };

    let string: string => t;
    let asString: t => option(string);
    let int: int => t;
    let asInt: t => option(int);
    let float: float => t;
    let asFloat: t => option(float);
    let bool: bool => t;
    let asBool: t => option(bool);
  };

  type t;

  let make: actorId => t;
  let change: (string, Json.Map.t => Json.Map.t, t) => t;
  let root: t => Json.Map.t;

  let getClock: t => Clock.t;
  let getChangesFromTime: (Clock.t, t) => ChangeSet.t;
  let applyChanges: (ChangeSet.t, t) => t;

  let save: t => string;
  let load: string => option(t);
};

module type Maker =
  (ActorId: Stringifiable) => CommonAPI with type actorId = ActorId.t;