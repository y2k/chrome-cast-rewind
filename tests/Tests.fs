module Tests

open System
open Xunit
open CastService

let timesData : obj array list =
    [ [| Service.period; 10.0 * Service.period; 20.0 * Service.period; TimeSpan.Zero; Service.period |]
      [| (2.0 * Service.period) - (Service.period / 2.0); 2.0 * Service.period; 4.0 * Service.period; TimeSpan.Zero; Service.period / 2.0 |]
      [| 3.0 * Service.period; 2.0 * Service.period; 4.0 * Service.period; 4.0 * Service.period; Service.period |] ]

[<Theory>]
[<MemberData "timesData">]
let test currentTime startTime endTime expSeekTime expSleepTime =
    let mutable seekTime = TimeSpan.Zero
    let mutable sleepTime = TimeSpan.Zero

    Service.main
        (async.Return <| Some { videoId = "1"; currentTime = currentTime })
        (Map.ofList [ "1", [| startTime, endTime |] ])
        (fun time -> seekTime <- time; async.Zero())
        (fun time -> sleepTime <- time; async.Zero())
    |> Async.RunSynchronously

    Assert.Equal(expSeekTime, seekTime)
    Assert.Equal(expSleepTime, sleepTime)
