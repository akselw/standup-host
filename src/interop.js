// This is called BEFORE your Elm app starts up
//
// The value returned here will be passed as flags
// into your `Shared.init` function.
export const flags = ({ env }) => {
    return { apiKey: env.SUPABASE_API_KEY };
};

// This is called AFTER your Elm app starts up
//
// Here you can work with `app.ports` to send messages
// to your Elm application, or subscribe to incoming
// messages from Elm
// export const onReady = ({ app, env }) => {
//
// }
