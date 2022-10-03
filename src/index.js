import { createClient } from '@supabase/supabase-js';
import { Elm } from './Main.elm';
import './styles/styles.scss';


const supabaseUrl = 'https://xluvzigagcthclpwrzhj.supabase.co';
const supabaseApiKey = process.env.SUPABASE_API_KEY;
const supabase = createClient(supabaseUrl, supabaseApiKey);


const app = Elm.Main.init({
    node: document.getElementById('app'),
});

const fetchTeam = async (shortname) => {
    const { data, error } = await supabase
        .from('team')
        .select()
        .match({ shortname: shortname })

    console.log({ data });
}

const b = fetchTeam('elm-2022');
