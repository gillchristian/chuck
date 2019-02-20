open Data;

type favorites = IntMap.t(Service.joke);
type jokes = list(Service.joke);

type state = {
  data: RemoteData.t(jokes),
  favorites,
  timer: ref(option(Js.Global.intervalId)),
  isTimerOn: bool,
};

type action =
  | FetchJokes
  | FetchJokesDone(RemoteData.t(jokes))
  | MarkFavorite(Service.joke)
  | RemoveFavorite(Service.joke)
  | StartTimer
  | Tick
  | StopTimer;

let persist_favorites = mfs =>
  Dom.Storage.(
    setItem(
      "chuck_favorites",
      mfs |> IntMap.values |> Service.encode_jokes |> Json.stringify,
      localStorage,
    )
  );

let make = _children => {
  ...ReasonReact.reducerComponent("Jokes"),
  initialState: () => {
    let favorites =
      switch (Dom.Storage.(getItem("chuck_favorites", localStorage))) {
      | Some(s) => s |> Json.parseOrRaise |> Service.decode_jokes
      | None => []
      };

    {
      data: RemoteData.NotAsked,
      favorites: IntMap.from_list(j => j.Service.id, favorites),
      timer: ref(None),
      isTimerOn: false,
    };
  },
  reducer: (action, state) =>
    switch (action) {
    | FetchJokes =>
      ReasonReact.UpdateWithSideEffects(
        {...state, data: RemoteData.Loading},
        self =>
          Js.Promise.(
            Service.fetch_jokes(10)
            |> then_(res =>
                 switch (res) {
                 | Service.Ok(d) =>
                   RemoteData.Success(d.Service.value) |> resolve
                 | Service.Fail(e) => RemoteData.Failure(e) |> resolve
                 }
               )
            |> then_(data => FetchJokesDone(data) |> self.send |> resolve)
            |> ignore
          ),
      )
    | FetchJokesDone(data) =>
      ReasonReact.Update({
        ...state,
        data:
          RemoteData.map(
            List.filter(j => !IntMap.mem(j.Service.id, state.favorites)),
            data,
          ),
      })
    | MarkFavorite(joke) =>
      IntMap.cardinal(state.favorites) >= 10 ?
        ReasonReact.NoUpdate :
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            data:
              RemoteData.map(
                List.filter(j => joke.id !== j.Service.id),
                state.data,
              ),
            favorites: IntMap.add(joke.id, joke, state.favorites),
          },
          ({state}) => persist_favorites(state.favorites),
        )
    | RemoveFavorite(joke) =>
      IntMap.mem(joke.Service.id, state.favorites) ?
        ReasonReact.UpdateWithSideEffects(
          {
            ...state,
            data:
              switch (state.data) {
              | RemoteData.Success(js) => RemoteData.Success([joke, ...js])
              | _ => RemoteData.Success([joke])
              },
            favorites: IntMap.remove(joke.id, state.favorites),
          },
          ({state}) => persist_favorites(state.favorites),
        ) :
        ReasonReact.NoUpdate
    | StartTimer =>
      IntMap.cardinal(state.favorites) >= 10 ?
        ReasonReact.NoUpdate :
        ReasonReact.UpdateWithSideEffects(
          {...state, isTimerOn: true},
          ({state, send}) =>
            state.timer :=
              Some(Js.Global.setInterval(() => send(Tick), 5000)),
        )
    | Tick =>
      IntMap.cardinal(state.favorites) >= 10 ?
        ReasonReact.SideEffects(({send}) => send(StopTimer)) :
        ReasonReact.SideEffects(
          self =>
            Js.Promise.(
              Service.fetch_jokes(1)
              |> then_(res =>
                   switch (res) {
                   | Service.Ok(d) => resolve(d.Service.value)
                   | Service.Fail(_) => resolve([])
                   }
                 )
              |> then_(js =>
                   switch (js) {
                   | [] => resolve(None)
                   | [j, ..._] => resolve(Some(j))
                   }
                 )
              |> then_(somefav =>
                   switch (somefav) {
                   | None => resolve()
                   | Some(fav) => MarkFavorite(fav) |> self.send |> resolve
                   }
                 )
              |> ignore
            ),
        )
    | StopTimer =>
      switch (state.timer^) {
      | None => ReasonReact.NoUpdate
      | Some(id) =>
        ReasonReact.UpdateWithSideEffects(
          {...state, timer: ref(None), isTimerOn: false},
          _self => Js.Global.clearInterval(id),
        )
      }
    },
  willUnmount: self => {
    switch (self.state.timer^) {
    | Some(id) => Js.Global.clearInterval(id)
    | None => ()
    };
  },
  render: ({send, state}) =>
    <div>
      <div className="section">
        <button
          className={
            RemoteData.is_loading(state.data) ?
              "button is-primary is-loading" : "button is-primary"
          }
          onClick={_ => send(FetchJokes)}
          disabled={RemoteData.is_loading(state.data)}>
          {ReasonReact.string("I want Chuck Norris Jokes!")}
        </button>
        {state.isTimerOn ?
           <button
             className="button is-danger" onClick={_ => send(StopTimer)}>
             {ReasonReact.string("Stop timer !!!")}
           </button> :
           <button
             className="button is-warning" onClick={_ => send(StartTimer)}>
             {ReasonReact.string("Start timer =D")}
           </button>}
      </div>
      <div className="section columns">
        <div className="column">
          {switch (state.data) {
           | RemoteData.NotAsked =>
             <div> {ReasonReact.string("What are you waiting for?")} </div>
           | RemoteData.Loading =>
             <div> {ReasonReact.string("Loading...")} </div>
           | RemoteData.Success(jokes) =>
             jokes
             |> List.map(j =>
                  <Joke
                    joke=j
                    handleClick={joke => send(MarkFavorite(joke))}
                    isFavorite=false
                  />
                )
             |> Array.of_list
             |> ReasonReact.array
           | RemoteData.Failure(_) =>
             <div> {ReasonReact.string("Ups ...")} </div>
           }}
        </div>
        <div className="column">
          <p className="is-size-5 has-text-weight-bold is-uppercase">
            {ReasonReact.string("Favorites")}
          </p>
          {state.favorites
           |> IntMap.values
           |> List.map(j =>
                <Joke
                  joke=j
                  handleClick={joke => send(RemoveFavorite(joke))}
                  isFavorite=true
                />
              )
           |> Array.of_list
           |> ReasonReact.array}
        </div>
      </div>
    </div>,
};