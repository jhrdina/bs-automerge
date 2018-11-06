let (|?>) = (o, fn) =>
  switch (o) {
  | None => None
  | Some(v) => fn(v)
  };

let (|?>>) = (o, fn) =>
  switch (o) {
  | None => None
  | Some(v) => Some(fn(v))
  };

let (|?) = (o, d) =>
  switch (o) {
  | None => d
  | Some(v) => v
  };