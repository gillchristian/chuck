module IntMap = {
  include Map.Make({
    type t = int;

    let compare = compare;
  });

  let values = m => fold((_key, v, vs) => [v, ...vs], m, []);

  let from_list = (f, xs) =>
    List.fold_right((i, m) => add(f(i), i, m), xs, empty);
};

module RemoteData = {
  type t('d) =
    | NotAsked
    | Loading
    | Success('d)
    | Failure(string);

  let is_loading = rd =>
    switch (rd) {
    | Loading => true
    | _ => false
    };

  let map = (f, rd) =>
    switch (rd) {
    | Success(d) => Success(f(d))
    | x => x
    };

  let chain = (f, rd) =>
    switch (rd) {
    | Success(d) => f(d)
    | x => x
    };
};

let rec range = (start, end_) =>
  if (start >= end_) {
    [];
  } else {
    [start, ...range(start + 1, end_)];
  };