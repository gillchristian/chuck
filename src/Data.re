module IntMap' =
  Map.Make({
    type t = int;

    let compare = compare;
  });

module IntMap = {
  include IntMap';

  let values = m => IntMap'.fold((_key, v, vs) => [v, ...vs], m, []);
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
};