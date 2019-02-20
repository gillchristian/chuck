open Data;

type favorites = IntMap.t(Service.joke);
type jokes = list(Service.joke);

type state = {
  data: RemoteData.t(jokes),
  favorites,
};

type action =
  | FetchJokes
  | FetchJokesDone(RemoteData.t(jokes))
  | MarkFavorite(Service.joke)
  | RemoveFavorite(Service.joke);

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
      switch (Dom.Storage.(getItem("chuck_favorites", sessionStorage))) {
      | Some(s) => s |> Json.parseOrRaise |> Service.decode_jokes
      | None => []
      };

    {
      data: RemoteData.NotAsked,
      favorites: IntMap.from_list(j => j.Service.id, favorites),
    };
  },
  reducer: (action, state) =>
    switch (action) {
    | FetchJokes =>
      ReasonReact.UpdateWithSideEffects(
        {...state, data: RemoteData.Loading},
        self =>
          Service.fetch_jokes(10)
          |> Js.Promise.then_(res =>
               (
                 switch (res) {
                 | Service.Ok(d) => RemoteData.Success(d.Service.value)
                 | Service.Fail(e) => RemoteData.Failure(e)
                 }
               )
               |> Js.Promise.resolve
             )
          |> Js.Promise.then_(data =>
               FetchJokesDone(data) |> self.send |> Js.Promise.resolve
             )
          |> ignore,
      )
    | FetchJokesDone(data) =>
      ReasonReact.Update({
        ...state,
        data:
          RemoteData.map(
            js =>
              List.filter(
                j => !IntMap.mem(j.Service.id, state.favorites),
                js,
              ),
            data,
          ),
      })
    | MarkFavorite(joke) =>
      IntMap.cardinal(state.favorites) >= 10 ?
        ReasonReact.NoUpdate :
        ReasonReact.UpdateWithSideEffects(
          {
            data:
              switch (state.data) {
              | RemoteData.Success(js) =>
                RemoteData.Success(
                  List.filter(j => joke.id !== j.Service.id, js),
                )
              | _ => RemoteData.Success([joke])
              },
            favorites: IntMap.add(joke.id, joke, state.favorites),
          },
          ({state}) => persist_favorites(state.favorites),
        )
    | RemoveFavorite(joke) =>
      IntMap.mem(joke.Service.id, state.favorites) ?
        ReasonReact.UpdateWithSideEffects(
          {
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
      </div>
      <div className="section">
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
    </div>,
};