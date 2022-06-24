# Standup Host

Hvem skal holde standup i dag?

https://standup-team-bruke.netlify.app

## Endre hvem som er i standup-rotasjonen

I filen [`src/Main.elm`](https://github.com/akselw/standup-host/blob/main/src/Main.elm#L45) er det en liste med navn. Det er denne som bestemmer hvem som er med i standup-rotasjonen.

Lag en commit direkte til master og push. Det trigger et bygg i Netlify, og du vil se endringen etter noen minutter.

## Kjøre lokalt

Dette behøver man ikke å gjøre for å endre lista, man vil se det på GitHub om det ikke bygger.
Hvis du allikevel vil kjøre appen lokalt så kan du gjøre følgende:

```
$ npm ci
$ npm start
```

