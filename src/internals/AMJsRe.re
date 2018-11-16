open InfixRe;

type uuid = string;

type op;
/*
 {
   obj: uuid
   action: link | ins | insert | set | remove | makeMap | makeText | makeList

   key: // nekdy u map, tez _head, nekdy tez elemId, napr. aaa:1
   index: int // nekdy u list
   value: // hodnota, na kterou se to meni

   type: list | map // nekdy
   path: array(string??) // nekdy

   elem: int
   actor: uuid
   seq: int
 }
 */

type opValue =
  | Number(float)
  | String(string)
  | Bool(bool)
  | Null;

let decodeOpValue = json =>
  switch (json |> Js.Json.decodeNumber) {
  | Some(v) => Some(Number(v))
  | None =>
    switch (json |> Js.Json.decodeString) {
    | Some(v) => Some(String(v))
    | None =>
      switch (json |> Js.Json.decodeBoolean) {
      | Some(v) => Some(Bool(v))
      | None =>
        switch (json |> Js.Json.decodeNull) {
        | Some(_) => Some(Null)
        | None => None
        }
      }
    }
  };

/* let decodeOp = json =>
   switch (json |> Js.Json.decodeObject) {
   | Some(dict) =>
     switch (
       dict |> Js.Dict.get("action") |?> Js.Json.decodeString,
       dict |> Js.Dict.get("obj") |?> Js.Json.decodeString,
       dict |> Js.Dict.get("key") |?> Js.Json.decodeString,
       dict |> Js.Dict.get("value") |?> decodeOpValue,
      ) {
      | (Some("set"), Some(obj), Some(key), Some(opValue)) =>
      }
   | None =>
   }; */

/* TODO: Finish proper parser */
let decodeOp: Js.Json.t => option(op) = json => Some(Obj.magic(json));

/*
 type insOp = {
   obj: uuid
   key: _head
   elem: int
 }

 makeMap = {
   obj: uuid
 }

 set = {
   obj: uuid
   key: string
   value: // hodnota
 }

 link = {
   obj: uuid,
   key: elemId
   value: uuid
 }

 Ins({
   obj: uuid,

 })
 */

[@bs.deriving abstract]
type change = {
  actor: uuid,
  seq: int,
  deps: Js.Dict.t(int),
  message: string,
  ops: array(op),
};
let decodeDeps: Js.Json.t => option(Js.Dict.t(int)) =
  json =>
    switch (json |> Js.Json.decodeObject) {
    | Some(dict) =>
      let invalid =
        dict->Js.Dict.keys
        |> Js.Array.some(key =>
             dict->Js.Dict.get(key) |?> Js.Json.decodeNumber == None
           );
      !invalid ? Some(Obj.magic(dict)) : None;
    | None => None
    };

let decodeArrayStrict:
  (Js.Json.t => option('a), Js.Json.t) => option(array('a)) =
  (itemDecoder, json) =>
    switch (json |> Js.Json.decodeArray) {
    | Some(arr) =>
      let invalid = arr |> Js.Array.some(item => item |> itemDecoder == None);
      !invalid ? Some(Obj.magic(arr)) : None;
    | None => None
    };

let decodeChange = json =>
  switch (json |> Js.Json.decodeObject) {
  | Some(dict) =>
    switch (
      dict->Js.Dict.get("actor") |?> Js.Json.decodeString,
      dict->Js.Dict.get("seq") |?> Js.Json.decodeNumber |?>> int_of_float,
      dict->Js.Dict.get("deps") |?> decodeDeps,
      dict->Js.Dict.get("message") |?> Js.Json.decodeString,
      dict->Js.Dict.get("ops") |?> decodeArrayStrict(decodeOp),
    ) {
    | (Some(actor), Some(seq), Some(deps), Some(message), Some(ops)) =>
      Some(change(~actor, ~seq, ~deps, ~message, ~ops))
    | _ => None
    }
  | None => None
  };

/* actorId -> suggested value */
type conflict = Js.Dict.t(Js.Json.t);
/* fieldName -> actorId -> suggested value */
type objectConflicts = Js.Dict.t(conflict);
/* index -> actorId -> suggested value */
type arrayConflicts = array(conflict);

[@bs.get]
external getArrayConflicts: array('a) => arrayConflicts = "_conflicts";

let decodeArrayItemsStrict = (itemDecoder, arr) => {
  let newArr = [||];
  let invalid =
    Js.Array.some(
      item =>
        switch (item |> itemDecoder) {
        | Some(decodedItem) =>
          newArr |> Js.Array.push(decodedItem) |> ignore;
          false;
        | None => true
        },
      arr,
    );
  !invalid ? Some(newArr) : None;
};

module ChangeSet = {
  type t = array(change);
  let toString: t => string = t => t |> Obj.magic |> Js.Json.stringify;
  let fromString = str =>
    switch (str |> Js.Json.parseExn) {
    | json =>
      json |> Js.Json.decodeArray |?> decodeArrayItemsStrict(decodeChange)
    | exception _ => None
    };
};

/*
 pole obcas indexovane jako [actorId]:[index], napr.
 "aaa:2"
  - mozna se tomu rika elemId
  - elemId = timestamp operace vytvoreni daneho prvku

 type elemId = {
   actorId: uuid,
   index: int
 };

 type arrayObj = {
   _init: op<makeList>, // operace, ktera ten objekt vytvorila (asi)
   _inbound: Set<op<link | ...>>,
   _elemIds: SkipList<elemId, {obj: uuid}>,



   _following: {
     [key]: List<op>, napr.
     _head: List<op<ins | ...>>
   },
   _maxElem: int,
   _insertion: {
     [elemId]: op<ins | ...>
   },
   [elemId]: List<op<link | ...>>
 }

 type mapObj = {
   // operace, ktera ten objekt vytvorila (asi)
   // [u rootu chybi]
   _init: op<makeMap>,
   // operace, ktere zpusobily vlozeni tohoto objektu do jineho
   // (do List nebo do Map)
   // [u rootu chybi]
   _inbound: Set<op<link | ...>>,
   [key]: List<op<set | link |...>>,
 }
 */

module Clock = {
  type t = ImmJsRe.Map.t(string, int);

  let lessOrEqual: (t, t) => bool =
    /* Taken from Automerge library */
    [%bs.raw
      (clock1, clock2) => "{
        return clock1.keySeq().concat(clock2.keySeq()).reduce(
          (result, key) => (result && clock1.get(key, 0) <= clock2.get(key, 0)),
          true,
        );
      }"
    ];
  /** [lessOrEqual(a, b)] returns true if all components of [a] are less than or equal to those of [b].

  Returns false if there is at least one component in which [a] is greater than [b] (that is, either clock1 is overall greater than clock2, or the clocks are incomparable).
  */
  [@bs.module "immutable"]
  external _unsafeClockFromDict:
    Js.Dict.t(Js.Json.t) => ImmJsRe.Map.t(string, int) =
    "Map";

  let toString = t => {
    let dict = Js.Dict.empty();
    t->ImmJsRe.Map.forEach((seq: int, actorId, _) =>
      dict->Js.Dict.set(actorId, Js.Json.number(float_of_int(seq)))
    );
    Js.Json.object_(dict) |> Js.Json.stringify;
  };

  let fromString = str =>
    switch (Js.Json.parseExn(str)) {
    | json =>
      switch (json |> Js.Json.decodeObject) {
      | Some(dict) =>
        let invalid =
          dict->Js.Dict.keys
          |> Js.Array.some(key =>
               dict->Js.Dict.get(key) |?> Js.Json.decodeNumber == None
             );
        !invalid ? Some(_unsafeClockFromDict(dict)) : None;

      | None => None
      }
    | exception _ => None
    };
};

type opSet;
/* = {
     states: {
       // Asi vsechny zmeny daneho actora
       [actorId]: [
         // Index corresponds to seq number of the change
         {
           change: change
           allDeps: {
             [actorId]: int
           }
         }
       ]
     },
     history: changeSet,
     // Contexts are not nested like in the original paper, but flattened
     // using object UUIDs.
     // This eliminates the need for Cursor and descending using cursor
     byObject: {
       // ROOT ma vetsinou 000000-0000...
       [objectId]: arrayObj | mapObj,
     },
     clock: clock,
     deps: {
       [actorId]: int
     },
     local: [],
     queue: [],
     cache: {
       [objectId]: object | list
     },


     undoPos: 0,
     undoLocal: [],
     undoStack: [],
     redoStack: [],
   } */

[@bs.deriving abstract]
type docState = {
  /* actorId: UUID? */
  opSet,
};
/*

 type object = {
   _conflicts: arrayConflicts,
   _objectId: uuid,
   [key]: whatever
 }

 type list = {
   _conflicts: arrayConflicts,
   _objectId: uuid,
   [index]: whatever
 }
 */

type t = Js.Dict.t(Js.Json.t);
/*
   {
     _actorId: UUID, // ID uzlu
     _state: docState,
     _objectId: UUID,
     _conflicts
   }
 */

type immChangeSet;

[@bs.module "automerge"] external init: unit => t = "";
[@bs.module "automerge"] external initWithActorId: uuid => t = "init";
[@bs.module "automerge"] external change: (t, string, t => unit) => t = "";

/* From automerge/auto_api */
[@bs.module "automerge"] external merge: (t, t) => t = "";
[@bs.module "automerge"] external getChanges: (t, t) => ChangeSet.t = "";
[@bs.module "automerge"] external diff: (t, t) => array(op) = "";

[@bs.module "automerge"] external save: t => Js.Json.t = "";
[@bs.module "automerge"] external load: (Js.Json.t, uuid) => option(t) = "";

[@bs.module "automerge"]
external _getMissingChanges: (opSet, Clock.t) => immChangeSet =
  "getMissingChanges";
let _getOpSet: t => opSet = [%raw doc => "{return doc._state.get('opSet')}"];
[@bs.send]
external _immToNormalChangeSet: immChangeSet => ChangeSet.t = "toJSON";
let getMissingChanges: (t, Clock.t) => ChangeSet.t =
  (doc, clock) =>
    doc->_getOpSet->_getMissingChanges(clock)->_immToNormalChangeSet;

[@bs.module "automerge"] external applyChanges: (t, ChangeSet.t) => t = "";

[@bs.module "automerge"]
external getConflicts: (t, ImmJsRe.List.t(change)) => objectConflicts = "";

let getClock: t => Clock.t = [%raw
  doc => "{return doc._state.get('opSet').get('clock')}"
];

let getActorId: t => uuid = [%raw doc => "{return doc._state.get('actorId')}"];
/*
 [@bs.module "automerge"] external canUndo: t => bool = "";
 [@bs.module "automerge"] external undo: (t, string) => t = "";
 [@bs.module "automerge"] external canRedo: t => bool = "";
 [@bs.module "automerge"] external redo: (t, string) => t = ""; */

/*

 DREAM API


 let schema = Automerge.Schema.(
   object([
     required("description", string),
     optionalWithConflictRes(
       "items",
       list(object([
         required("sss", int),
         optional("boooo", bool)
       ])),
       atomic
     )
   ])
 );

 Conflict resolution */

/* solve = (resolve) */