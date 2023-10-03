import { createClient } from "@supabase/supabase-js"

// This is called BEFORE your Elm app starts up
//
// The value returned here will be passed as flags
// into your `Shared.init` function.
export const flags = ({ env }) => ({
    apiKey: env.SUPABASE_API_KEY,
    accessToken: localStorage.getItem("hvem-har-standup:access_token"),
})

const isProd = () => {
    try {
        return !new URL(window.location.href).host.includes("localhost")
    } catch {
        return true
    }
}

// This is called AFTER your Elm app starts up
//
// Here you can work with `app.ports` to send messages
// to your Elm application, or subscribe to incoming
// messages from Elm
export const onReady = async ({ app, env }) => {
    const supabaseUrl = "https://xluvzigagcthclpwrzhj.supabase.co"
    const supabaseKey = env.SUPABASE_API_KEY
    const supabase = createClient(supabaseUrl, supabaseKey)

    app.ports.authentication.subscribe(async (action) => {
        switch (action.type) {
            case "LOGIN": {
                const hostname = isProd()
                    ? "https://hvemharstandup.no/oauth"
                    : "http://localhost:1234/oauth"
                let { data, error } = await supabase.auth.signInWithOAuth({
                    provider: "github",
                    options: {
                        redirectTo:
                            hostname +
                            (action.redirectUrl
                                ? `?redirect=${action.redirectUrl}`
                                : ""),
                    },
                })
                return
            }
            default:
                return
        }
    })
    app.ports.localStorage.subscribe((action) => {
        switch (action.type) {
            case "SET_ITEM":
                // TODO: Endre til at key her er en JS-const, samme som brukes i flags
                localStorage.setItem(action.key, action.value)
                return
            case "REMOVE_ITEM":
                // TODO: Endre til at key her er en JS-const, samme som brukes i flags
                localStorage.setItem(action.key, action.value)
                return
        }
    })
    // let { data, error } = await supabase.auth.signInWithOAuth({
    //     provider: 'github'
    //     , options: { redirectTo: 'http://localhost:1234/min-side'}
    // })
    // console.log({ data , error });
}
