module CastService

open System

module Types =
    type Status =
        { videoId: string
          currentTime: TimeSpan }

module Service =
    open Types

    type Cmd = SeekToTime of TimeSpan | Sleep of TimeSpan

    let period = TimeSpan.FromSeconds 5.0

    let private handleGenericTime times targetTime handleTime =
        times
        |> Array.tryFind (fun (s, e) -> targetTime >= s && targetTime <= e)
        |> Option.map handleTime

    let private handleFutureTime (status: Status) times =
        handleGenericTime times (status.currentTime + period) (fun (s, _) ->
            [ Sleep (s - status.currentTime) ])

    let private handleTimes (status: Status) times =
        handleGenericTime times status.currentTime (fun (_, e) ->
            [ SeekToTime e; Sleep period ])

    let makeCommandsForStatus optStatus times =
        let handleStatus status =
            Map.tryFind status.videoId times
            |> Option.bind ^ fun times ->
                handleTimes status times
                |> Option.orElse (handleFutureTime status times)

        optStatus
        |> Option.bind handleStatus
        |> Option.defaultValue [ Sleep period ]

    let main getStatus times seek sleep =
        async {
            let! status = getStatus
            let cmds = makeCommandsForStatus status times
            printfn "Cmds: %A" cmds

            for cmd in cmds do
                match cmd with
                | SeekToTime time -> do! seek time
                | Sleep time -> do! sleep time
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

module Database =
    open System.Collections.Generic
    open System.Net
    open System.Text.Json

    let download (url: string) =
        use client = new WebClient()

        let dictionary : Dictionary<string, {| from: double; ``to``: double |} []> =
            client.DownloadString url
            |> JsonSerializer.Deserialize

        dictionary
        |> Seq.map (fun x ->
            x.Key
            , x.Value
              |> Array.map (fun x -> TimeSpan.FromSeconds x.from, TimeSpan.FromSeconds x.``to``))
        |> Map.ofSeq

[<EntryPoint>]
let main argv =
    async {
        let! t = ChromeCast.make

        let times =
            Database.download "https://raw.githubusercontent.com/y2k/chrome-cast-rewind-database/master/v1/ads.json"

        while true do
            do! Service.main (ChromeCast.getStatus t) times (ChromeCast.seek t) Async.Sleep
    } |> Async.RunSynchronously
    0
