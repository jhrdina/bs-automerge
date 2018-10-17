module type DocType = {type t;};

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

[@bs.deriving abstract]
type change = {
  actor: uuid,
  seq: int,
  /* deps: {
       [actorId]: int
     } */
  message: string,
  ops: array(op),
};

/* actorId -> suggested value */
type conflict = Js.Dict.t(Js.Json.t);
/* fieldName -> actorId -> suggested value */
type objectConflicts = Js.Dict.t(conflict);
/* index -> actorId -> suggested value */
type arrayConflicts = array(conflict);

[@bs.get]
external getArrayConflicts: array('a) => arrayConflicts = "_conflicts";

type changeSet = array(change);

module Make = (Doc: DocType) => {
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

  type clock;
  /* = {
       [actorId]: int
     }
     */

  type opSet;
  /* = {
       states: {
         // Asi vsechny zmeny daneho actora
         [actorId]: [
           {
             change: change
             allDeps: {
               [actorId]: int
             }
           }
         ]
       },
       history: changeSet,
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

  type t = Doc.t;
  /*
     {
       _actorId: UUID, // ID uzlu
       _state: docState,
       _objectId: UUID,
       _conflicts
     }
   */

  /* [@bs.module "automerge"] external init: unit => t = "initImmutable"; */
  [@bs.module "automerge"] external init: (~actorId: uuid=?, unit) => t = "";
  [@bs.module "automerge"] external change: (t, string, t => unit) => t = "";

  /* From automerge/auto_api */
  [@bs.module "automerge"] external merge: (t, t) => t = "";
  [@bs.module "automerge"] external getChanges: (t, t) => changeSet = "";
  [@bs.module "automerge"] external diff: (t, t) => array(op) = "";

  [@bs.module "automerge"] external save: t => string = "";
  [@bs.module "automerge"]
  external load: (string, ~actorId: uuid=?) => string = "";
  [@bs.module "automerge"]
  external _getMissingChanges: (opSet, clock) => changeSet =
    "getMissingChanges";
  let _getOpSet: t => opSet = [%raw doc => "{return doc._state.get('opSet')}"];
  let getMissingChanges: (t, clock) => changeSet =
    (doc, clock) => doc->_getOpSet->_getMissingChanges(clock);

  [@bs.module "automerge"] external applyChanges: (t, changeSet) => t = "";

  [@bs.module "automerge"]
  external getConflicts: (t, ImmJsRe.List.t(change)) => objectConflicts = "";

  let getClock: t => clock = [%raw
    doc => "{return doc._state.get('opSet').get('clock')}"
  ];
  /*
   [@bs.module "automerge"] external canUndo: t => bool = "";
   [@bs.module "automerge"] external undo: (t, string) => t = "";
   [@bs.module "automerge"] external canRedo: t => bool = "";
   [@bs.module "automerge"] external redo: (t, string) => t = ""; */
};

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