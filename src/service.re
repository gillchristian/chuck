/* decoding */
type joke = {
  id: int,
  content: string,
};

type response = {value: list(joke)};

let decode_entry = (json): joke => {
  id: Json.Decode.field("id", Json.Decode.int, json),
  content: Json.Decode.field("joke", Json.Decode.string, json),
};

let decode_response = json => {
  value: Json.Decode.field("value", Json.Decode.list(decode_entry), json),
};

/* TODO: failure decoder */
let decore_error_msg = json =>
  Json.Decode.field("error", Json.Decode.string, json);

/* fetch */

type result('a, 'b) =
  | Ok('a)
  | Fail('b);

let something_went_wrong = _err =>
  Fail("Something went wront...") |> Js.Promise.resolve;

let handle_response = res =>
  Js.Promise.(
    if (Fetch.Response.ok(res)) {
      res |> Fetch.Response.json |> then_(json => resolve(Ok(json)));
    } else {
      res |> Fetch.Response.json |> then_(json => resolve(Fail(json)));
    }
  );

let fetch = (url, success_decoder) =>
  Fetch.fetch(url)
  |> Js.Promise.then_(handle_response)
  |> Js.Promise.then_(either_json =>
       Js.Promise.resolve(
         switch (either_json) {
         | Ok(res) => Ok(success_decoder(res))
         | Fail(json) => Fail(decore_error_msg(json))
         },
       )
     )
  |> Js.Promise.catch(something_went_wrong);

/* fetch methods */

let fetch_jokes = count => {
  let url = "http://api.icndb.com/jokes/random/" ++ string_of_int(count);

  fetch(url, decode_response);
};

/* remote data */

type remote_data('d) =
  | NotAsked
  | Loading
  | Success('d)
  | Failure(string);

let is_loading = rd =>
  switch (rd) {
  | Loading => true
  | _ => false
  };