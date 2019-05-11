# bs-automerge

This library tries to bring CRDTs (conflict-free replicated datatypes) to OCaml/ReasonML.

It contains bindings to [Automerge](https://github.com/automerge/automerge) library and an attempt to do a pure functional Reason rewrite of the [original paper](https://www.cl.cam.ac.uk/~arb33/papers/KleppmannBeresford-CRDT-JSON-TPDS2017.pdf).

It provides three APIs:

- **Tight Automerge binding** (`Automerge.Js`) - API that closely copies the usage experience of the original JavaScript library, including mutations. See [example](examples/TightJsApiDemo.re).

- **Automerge binding with unified API** (`Automerge.UniJs`) - API that emulates more idiomatic functional usage (has no visible mutations). Interface should be universal enough to stay the same even for an eventual purely functional implementation. See [example](examples/UnifiedApi.re)

- **Purely functional implementation** (`Automerge.Pure`) - Experimental implementation of the [original paper](https://www.cl.cam.ac.uk/~arb33/papers/KleppmannBeresford-CRDT-JSON-TPDS2017.pdf). It is much smaller than Automerge, however it lacks a lot of features and performance improvements Automerge added on top of the article. See [example](examples/PureImplementation.re).

## Installation

Add the library to your project using

```
npm install bs-automerge
```

and add a corresponding item to your `bsconfig.json`:

```json
"bs-dependencies": ["bs-automerge"],
```

See examples above for usage.

## Building

### Build

```
npm run build
```

### Build + Watch

```
npm run start
```

## Running examples

After building the project (see above), you can run the examples in the project root folder simply by

```
node lib/js/examples/UnifiedApi.js
```

and replace `UnifiedApi` with any other example name you wish to run. You can find their implementations in the [examples](examples) folder.
