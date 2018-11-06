type replicaId = string;
module ReplicaIdMap = Map.Make(String);

type timestamp = (int, replicaId);
let encodeTimestamp = ((i, rep)) =>
  "TS(" ++ string_of_int(i) ++ ", " ++ rep ++ ")";

module Timestamp = {
  type t = timestamp;
  let compare = compare;
};
module TimestampMap = Map.Make(Timestamp);
/* VAL */
type value =
  | String(string)
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Null
  | EmptyObject
  | EmptyArray;
let encodeValue =
  fun
  | String(v) => "String(" ++ v ++ ")"
  | Int(v) => "Int(" ++ string_of_int(v) ++ ")"
  | Float(v) => "Float(" ++ string_of_float(v) ++ ")"
  | Bool(v) => "Bool(" ++ string_of_bool(v) ++ ")"
  | Null => "Null"
  | EmptyObject => "EmptyObject"
  | EmptyArray => "EmptyArray";

type mut =
  | Assign(value)
  | Insert(value)
  | Delete;
let encodeMut =
  fun
  | Assign(v) => "Assign(" ++ encodeValue(v) ++ ")"
  | Insert(v) => "Insert(" ++ encodeValue(v) ++ ")"
  | Delete => "Delete";

type cursorItem =
  | Doc
  /* !! Custom */
  | Str(string)
  /* Inside list */
  | Head
  | Tail
  | Timestamp(timestamp);
let encodeCursorItem =
  fun
  | Doc => "Doc"
  | Str(s) => "Str('" ++ s ++ "')"
  | Head => "Head"
  | Tail => "Tail"
  | Timestamp(t) => t |> encodeTimestamp;

module CursorItem = {
  type t = cursorItem;
  let compare = compare;
};
module CursorItemMap = Map.Make(CursorItem);
module CursorItemSet = Set.Make(CursorItem);

type typedKey =
  /* Map - in ctx uses: items, pres */
  | MapT(cursorItem)
  /* List - in ctx uses: items, next, pres */
  | ListT(cursorItem)
  /* Register - in ctx uses: values */
  | RegT(cursorItem);

let encodeTypedKey =
  fun
  | MapT(ci) => "Map(" ++ encodeCursorItem(ci) ++ ")"
  | ListT(ci) => "List(" ++ encodeCursorItem(ci) ++ ")"
  | RegT(ci) => "Reg(" ++ encodeCursorItem(ci) ++ ")";

module TypedKey = {
  type t = typedKey;
  let compare = compare;
};

module TypedKeyMap = Map.Make(TypedKey);

type cursor =
  | Cursor(list(typedKey), cursorItem);
let encodeCursor = (Cursor(items, kn)) =>
  "Cursor("
  ++ String.concat(" -> ", items |> List.map(encodeTypedKey))
  ++ ", "
  ++ encodeCursorItem(kn)
  ++ ")";

type op = {
  id: timestamp,
  /* Set of causal dependencies:
     all operations from any replica that had been applied on this replica before this operation */
  /* TODO: Convert to set */
  deps: list(timestamp),
  cur: cursor,
  mut,
};

let encodeOp = op =>
  Json.Object([
    ("id", Json.String(op.id |> encodeTimestamp)),
    (
      "deps",
      Json.Array(
        op.deps |> List.map(ts => Json.String(encodeTimestamp(ts))),
      ),
    ),
    ("cur", Json.String(op.cur |> encodeCursor)),
    ("mut", Json.String(op.mut |> encodeMut)),
  ]);

type ctx = {
  items: TypedKeyMap.t(ctx),
  next: CursorItemMap.t(cursorItem),
  /* Presence set: set of all operations that have asserted the existence of
     this list element

     Note: In Automerge called '_inbound' in opSet.byObject[id]
     */
  /* TODO: check type, maybe cursorItem */
  /* TODO: Convert to real set to prevent duplicates */
  pres: CursorItemMap.t(list(timestamp)),
  values: TimestampMap.t(value),
};
let rec encodeCtx = ({items, next, pres, values}) => {
  let encodeItems = items =>
    Json.Object(
      TypedKeyMap.fold(
        (typedKey, ctx, acc) => [
          (typedKey |> encodeTypedKey, ctx |> encodeCtx),
          ...acc,
        ],
        items,
        [],
      ),
    );
  let encodeNext = next =>
    Json.Object(
      CursorItemMap.fold(
        (cursorItemA, cursorItemB, acc) => [
          (
            cursorItemA |> encodeCursorItem,
            Json.String(cursorItemB |> encodeCursorItem),
          ),
          ...acc,
        ],
        next,
        [],
      ),
    );
  let encodePres = pres =>
    Json.Object(
      CursorItemMap.fold(
        (ci, tss, acc) => [
          (
            ci |> encodeCursorItem,
            Json.Array(
              tss |> List.map(ts => Json.String(encodeTimestamp(ts))),
            ),
          ),
          ...acc,
        ],
        pres,
        [],
      ),
    );
  let encodeValues = values =>
    Json.Object(
      TimestampMap.fold(
        (ts, value, acc) => [
          (ts |> encodeTimestamp, Json.String(encodeValue(value))),
          ...acc,
        ],
        values,
        [],
      ),
    );
  Json.Object([
    ("items", items |> encodeItems),
    ("next", next |> encodeNext),
    ("pres", pres |> encodePres),
    ("values", values |> encodeValues),
  ]);
};

let emptyCtx = {
  items: TypedKeyMap.empty,
  next: CursorItemMap.empty,
  pres: CursorItemMap.empty,
  values: TimestampMap.empty,
};

/* TODO: Convert to an actual Set */
type opSet = list(op);
/* Extra */
type change = {
  id: timestamp,
  message: string,
  /* Why not TimestampSet? */
  ops: opSet,
};
type state = {
  replicaId,
  queue: opSet,
  /* TODO: Convert to Set */
  ops: list(timestamp),
  ctx,
  recv: opSet,
  /* Extra */
  changes: ReplicaIdMap.t(TimestampMap.t(change)),
};

let encodeState = ({queue, ops, ctx}) =>
  Json.Object([
    ("queue", Json.Array(queue |> List.map(encodeOp))),
    (
      "ops",
      Json.Array(ops |> List.map(ts => Json.String(encodeTimestamp(ts)))),
    ),
    ("ctx", ctx |> encodeCtx),
  ]);

/* type ctxVal =
   | Term
   | Nonterm(ctxVal); */

/* ======================================= */
/* helpers */

/* ======================================= */
/* let _exprToCursor = (expr) => */
/* EXPR */
let doc = Cursor([], Doc);
let get = (cur, key) => {
  let Cursor(tail, head) = cur;
  switch (head) {
  | Head => raise(Not_found)
  | others => Cursor(List.append(tail, [MapT(others)]), Str(key))
  };
};

let idx = (cur, i, state) => {
  let rec idx_i = (cur, ctx: ctx, i) =>
    switch (cur) {
    /* IDX2 */
    | Cursor([k1, ...tail], kn) =>
      let Cursor(_, kn_new) =
        idx_i(Cursor(tail, kn), ctx.items |> TypedKeyMap.find(k1), i);
      Cursor([k1, ...tail], kn_new);
    | Cursor([], k) =>
      if (i === 0) {
        /* IDX5 */
        Cursor([], k);
      } else if (i > 0) {
        let k_new = ctx.next |> CursorItemMap.find(k);
        if (k_new != Tail) {
          if (ctx.pres |> CursorItemMap.find(k_new) != []) {
            /* IDX3 */
            /* Skipping over an item */
            idx_i(
              Cursor([], k_new),
              ctx,
              i - 1,
            );
          } else {
            /* IDX4 */
            /* Skipping over a deleted item */
            idx_i(
              Cursor([], k_new),
              ctx,
              i,
            );
          };
        } else {
          raise(Not_found);
        };
      } else {
        raise(Not_found);
      }
    };
  /* IDX1 */
  let Cursor(tail, kn) = cur;
  Cursor(List.append(tail, [ListT(kn)]), Head)->idx_i(state.ctx, i);
};

/* keys */
let keysOfContext = ctx =>
  TypedKeyMap.fold(
    (MapT(k) | ListT(k) | RegT(k), _v, acc) => [k, ...acc],
    ctx.items,
    [],
  );

let keys = (cur, state) => {
  let rec keys_i = (ctx: ctx) =>
    fun
    /* KEYS3 */
    | Cursor([k1, ...tail], kn) when ctx.items |> TypedKeyMap.mem(k1) =>
      keys_i(ctx.items |> TypedKeyMap.find(k1), Cursor(tail, kn))
    | Cursor([_, ..._], _) => raise(Not_found)
    /* KEYS2 */
    | Cursor([], k) => {
        let map = ctx.items |> TypedKeyMap.find(MapT(k));
        keysOfContext(map)
        |> List.filter(k => map.pres |> CursorItemMap.find(k) != []);
      };
  /* KEYS1 */
  keys_i(state.ctx, cur);
};

/* ???? */
let range = ctx => ctx.values;

let val_ = (cur, state) => {
  let rec val_i = ctx =>
    fun
    /* VAL3 */
    | Cursor([k1, ...tail], kn) when ctx.items |> TypedKeyMap.mem(k1) =>
      val_i(ctx.items |> TypedKeyMap.find(k1), Cursor(tail, kn))
    | Cursor([_, ..._], _) => raise(Not_found)
    /* VAL2 */
    | Cursor([], k) => range(ctx.items |> TypedKeyMap.find(RegT(k)));
  /* VAL1 */
  val_i(state.ctx, cur);
};

/* Operations */

type addId =
  | AddId(typedKey, timestamp, mut);

let presence = (ctx, k) =>
  switch (ctx.pres |> CursorItemMap.find(k)) {
  /* PRESENCE1 */
  | value => value
  /* PRESENCE2 */
  | exception Not_found => []
  };

let addId = ctx =>
  fun
  | AddId(_kTag, _id, Delete) => ctx
  | AddId(MapT(k) | ListT(k) | RegT(k), id, _mut) => {
      ...ctx,
      pres: ctx.pres |> CursorItemMap.add(k, [id, ...presence(ctx, k)]),
    };

let child = ctx =>
  fun
  /* CHILD-GET */
  | k when ctx.items |> TypedKeyMap.mem(k) =>
    ctx.items |> TypedKeyMap.find(k)
  /* CHILD-MAP */
  | MapT(_k) => emptyCtx
  /* CHILD-LIST */
  | ListT(_k) => {
      ...emptyCtx,
      next: emptyCtx.next |> CursorItemMap.add(Head, Tail),
    }
  /* CHILD-REG */
  | RegT(_k) => emptyCtx;

let rec clearAny = (ctx, deps, k) => {
  let (ctx1, pres1) = clear(ctx, deps, MapT(k));
  let (ctx2, pres2) = clear(ctx1, deps, ListT(k));
  let (ctx3, pres3) = clear(ctx2, deps, RegT(k));
  (ctx3, List.concat([pres1, pres2, pres3]));
}
and clearElem = (ctx, deps, k) => {
  let (ctx', pres1) = clearAny(ctx, deps, k);
  let pres2 = presence(ctx', k);
  let pres3 =
    List.append(pres1, pres2) |> List.filter(i => !List.mem(i, deps));
  ({...ctx', pres: ctx'.pres |> CursorItemMap.add(k, pres3)}, pres3);
}
and clearMap = (ctx, deps, _done) => {
  /* !! Customized */
  let rec clearMap_i = (ctx, deps) =>
    fun
    /* CLEAR-MAP2 */
    | [k, ...tail] => {
        let (ctx', pres1) = clearElem(ctx, deps, k);
        let (ctx'', pres2) = clearMap_i(ctx', deps, tail);
        (ctx'', List.append(pres1, pres2));
      }
    | [] => (ctx, [] /* set */);
  clearMap_i(ctx, deps, keysOfContext(ctx));
}
and clearList = (ctx, deps) =>
  fun
  /* CLEAR-LIST2 */
  | k when k !== Tail => {
      let next = ctx.next |> CursorItemMap.find(k);
      let (ctx', pres1) = clearElem(ctx, deps, k);
      let (ctx'', pres2) = clearList(ctx', deps, next);
      (ctx'', List.append(pres1, pres2));
    }
  /* CLEAR-LIST3 */
  | _ => (ctx, [] /* set */)
and clear = (ctx, deps) =>
  fun
  /* CLEAR-NONE */
  | k when !(ctx.items |> TypedKeyMap.mem(k)) => (ctx, [])
  /* CLEAR-REG */
  | RegT(k) => {
      let concurrent = {
        ...emptyCtx,
        values:
          (ctx.items |> TypedKeyMap.find(RegT(k))).values
          |> TimestampMap.filter((id, _v) => !List.mem(id, deps)),
      };
      (
        {...ctx, items: ctx.items |> TypedKeyMap.add(RegT(k), concurrent)},
        TimestampMap.fold(
          (id, _v, pres) => [id, ...pres],
          concurrent.values,
          [],
        ),
      );
    }
  /* CLEAR-MAP1 */
  | MapT(k) => {
      /* TODO: Really []? */
      let (cleared, pres) =
        clearMap(
          ctx.items |> TypedKeyMap.find(MapT(k)),
          deps,
          [] /* set */,
        );
      (
        {...ctx, items: ctx.items |> TypedKeyMap.add(MapT(k), cleared)},
        pres,
      );
    }
  /* CLEAR-LIST1 */
  | ListT(k) => {
      let (cleared, pres) =
        clearList(ctx.items |> TypedKeyMap.find(ListT(k)), deps, Head);
      (
        {...ctx, items: ctx.items |> TypedKeyMap.add(ListT(k), cleared)},
        pres,
      );
    };

let rec _applyOp = ctx =>
  fun
  /* DESCEND */
  | {id, deps, cur: Cursor([k1, ...tail], kn), mut} => {
      let child = child(ctx, k1);
      let child' = _applyOp(child, {id, deps, cur: Cursor(tail, kn), mut});
      let ctx' = addId(ctx, AddId(k1, id, mut));
      {...ctx', items: ctx'.items |> TypedKeyMap.add(k1, child')};
    }
  /* ASSIGN */
  | {id, deps, cur: Cursor([], k), mut: Assign(value)}
      when value !== EmptyArray && value !== EmptyObject => {
      let (ctx', _pres) = clear(ctx, deps, RegT(k));
      let ctx'' = addId(ctx', AddId(RegT(k), id, Assign(value)));
      let child = child(ctx'', RegT(k));
      {
        ...ctx'',
        items:
          ctx''.items
          |> TypedKeyMap.add(
               RegT(k),
               {
                 ...child,
                 values: child.values |> TimestampMap.add(id, value),
               },
             ),
      };
    }
  /* EMPTY-MAP */
  | {id, deps, cur: Cursor([], k), mut: Assign(value)}
      when value === EmptyObject => {
      let (ctx', _pres) = clearElem(ctx, deps, k);
      let ctx'' = addId(ctx', AddId(MapT(k), id, Assign(value)));
      let child = child(ctx'', MapT(k));
      {...ctx'', items: ctx''.items |> TypedKeyMap.add(MapT(k), child)};
    }
  /* EMPTY-LIST */
  | {id, deps, cur: Cursor([], k), mut: Assign(value)}
      when value === EmptyArray => {
      let (ctx', _pres) = clearElem(ctx, deps, k);
      let ctx'' = addId(ctx', AddId(ListT(k), id, Assign(value)));
      let child = child(ctx'', ListT(k));
      {...ctx'', items: ctx''.items |> TypedKeyMap.add(ListT(k), child)};
    }
  | {id, deps, cur: Cursor([], prev), mut: Insert(value)} => {
      let next = ctx.next |> CursorItemMap.find(prev);
      if (switch (next) {
          | Timestamp(n) when n < id => true
          | Tail => true
          | _ => false
          }) {
        /* INSERT1 */
        let ctx' =
          _applyOp(
            ctx,
            {id, deps, cur: Cursor([], Timestamp(id)), mut: Assign(value)},
          );
        {
          ...ctx',
          next:
            ctx'.next
            |> CursorItemMap.add(prev, Timestamp(id))
            |> CursorItemMap.add(Timestamp(id), next),
        };
      } else if (switch (next) {
                 | Timestamp(n) when id < n => true
                 | _ => false
                 }) {
        /* INSERT2 */
        _applyOp(
          ctx,
          {id, deps, cur: Cursor([], Timestamp(id)), mut: Insert(value)},
        );
      } else {
        raise(Not_found);
      };
    }
  | {id: _, deps, cur: Cursor([], k), mut: Delete} =>
    clearElem(ctx, deps, k) |> fst
  | {cur: Cursor([], _), mut: Assign(_), _} => raise(Not_found);

/* APPLY-LOCAL */
let apply = (state, op) => {
  ...state,
  ctx: _applyOp(state.ctx, op),
  queue: [op, ...state.queue],
  ops: [op.id, ...state.ops],
};

let listFindOpt = (f, l) =>
  switch (List.find(f, l)) {
  | a => Some(a)
  | exception Not_found => None
  };

/* APPLY-REMOTE */
let recv = (ops, state) => {
  let opHasFulfilledDeps = (state, op) =>
    listFindOpt(dep => !List.mem(dep, state.ops), op.deps) == None;

  let state = {...state, recv: state.recv |> List.append(ops)};

  /* TODO: Throw away
       - ops with ids that I already have
       - ops with my replicaId
     */
  let rec inter = state => {
    let maybeOp =
      state.recv
      |> listFindOpt((op: op) =>
           !List.mem(op.id, state.ops) && opHasFulfilledDeps(state, op)
         );

    switch (maybeOp) {
    | Some(op) =>
      inter({
        ...state,
        ctx: _applyOp(state.ctx, op),
        ops: [op.id, ...state.ops],
        recv: state.recv |> List.filter((oldOp: op) => oldOp.id != op.id),
      })
    | None => state
    };
  };
  inter(state);
};

/* TODO: Allow getting ops only from certain timestamp */
let send = state => state.queue;

let makeOp = (state, cur, mut) => {
  /* TODO: do something */
  let p = state.replicaId;
  let ctr =
    state.ops
    |> List.map(((c, _)) => c)
    |> List.fold_left(
         (max, i) =>
           if (i > max) {
             i;
           } else {
             max;
           },
         0,
       );
  apply(state, {id: (ctr + 1, p), deps: state.ops, cur, mut});
};

let makeAssign = (cur, value, state) => makeOp(state, cur, Assign(value));
let makeInsert = (cur, value, state) => makeOp(state, cur, Insert(value));
let makeDelete = (cur, state) => makeOp(state, cur, Delete);

let make = replicaId => {
  replicaId,
  queue: [],
  ops: [],
  ctx: emptyCtx,
  recv: [],
  changes: ReplicaIdMap.empty,
};

let getReplicaChanges = (replicaId, state) =>
  switch (state.changes |> ReplicaIdMap.find(replicaId)) {
  | replicaChanges => replicaChanges
  | exception Not_found => TimestampMap.empty
  };

/* let makeChangeOfDiff = (ctx, newCtx, state) => {
     switch(ctx, newCtx) {
       | ()
       /* TODO: WRITE HERE <<<<<<< */
     }
   } */

let change = (message, f, state) => {
  let replicaChanges = state |> getReplicaChanges(state.replicaId);
  {
    ...state,
    changes:
      state.changes |> ReplicaIdMap.add(state.replicaId, replicaChanges),
  };
};