open System

module Types =
    type Status =
        { videoId: string
          currentTime: TimeSpan }

module Service =
    open Types

    type Cmd = SendNewTime of TimeSpan | SleepWith of TimeSpan

    let private handleGenericTime (times: (TimeSpan * TimeSpan) list) targetTime handleTime =
        times
        |> List.tryFind (fun (s, e) -> targetTime >= s && targetTime <= e)
        |> Option.map handleTime

    let private handleFutureTime (status: Status) times period =
        handleGenericTime times (status.currentTime + period) (fun (s, _) ->
            [ SleepWith (s - status.currentTime) ])

    let private handleTimes (status: Status) times period =
        handleGenericTime times status.currentTime  (fun (_, e) ->
            [ SendNewTime e; SleepWith period ])

    let makeCommandsForStatus optStatus times period =
        let handleSesion status =
            Map.tryFind status.videoId times
            |> Option.bind ^ fun times ->
                handleTimes status times period
                |> Option.orElse (handleFutureTime status times period)

        optStatus
        |> Option.bind handleSesion
        |> Option.defaultValue [ SleepWith period ]

    let main getStatus times seek =
        async {
            while true do
                let! status = getStatus
                let cmds = makeCommandsForStatus status times (TimeSpan.FromSeconds 5.0)
                printfn "Cmds: %A" cmds
                for cmd in cmds do
                    match cmd with
                    | SendNewTime time -> do! seek time
                    | SleepWith time -> do! Async.Sleep time
        }

module ChromeCast =
    open GoogleCast
    open GoogleCast.Channels
    open Types

    type t = { sender: Sender }

    let make =
        async {
            let! receivers = (new DeviceLocator()).FindReceiversAsync()
            let receiver = Seq.head receivers
            let sender = new Sender()
            let! _ = sender.ConnectAsync(receiver) |> Async.AwaitTask
            return { sender = sender }
        }

    let getStatus (t: t) =
        async {
            let mediaChannel = t.sender.GetChannel<IMediaChannel>()
            let! stat = mediaChannel.GetStatusAsync() |> Async.AwaitTask |> Async.catch
            return
                stat
                |> Result.map (fun stat ->
                    { videoId = stat.Media.ContentId
                      currentTime = TimeSpan.FromSeconds stat.CurrentTime })
                |> function Ok x -> Some x | Error _ -> None
        }

    let seek (t: t) (time: TimeSpan) =
        async {
            let mediaChannel = t.sender.GetChannel<IMediaChannel>()
            let! _ = mediaChannel.SeekAsync time.TotalSeconds
            return ()
        }

[<EntryPoint>]
let main argv =
    async {
        let! t = ChromeCast.make

        let times =
            Map.ofList [
                "Le3564euPGQ", [ TimeSpan.FromSeconds 420.0, TimeSpan.FromSeconds 508.0 ]
            ]

        do! Service.main (ChromeCast.getStatus t) times (ChromeCast.seek t)
    } |> Async.RunSynchronously
    0
