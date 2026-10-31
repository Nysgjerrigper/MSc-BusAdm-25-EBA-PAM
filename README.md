# Database for Oppgave R og Datasett

## Mappeinnhold
- **Datasett:** Selve Fantasy Premier Leauge datasettene som er hentet fra Vaastav sin Github database og out-of-sample prediksjonene til prognosemodellen.
- **MILP Py:** Blandet Heltallsprogrammene kopiert fra Kristiansen et al. i Python, samt .bat filer som gjør at man kan kjøre MILP skriptsa i løkke med forskjellige del-horisonter.
    - t := test/eksperimentell, auto := skript som har mulighet for å kjøres gjennom med flere horisonter. Kjøres med .bat filene, rediger .bat filene for å endre på hvor mange ganger man ønsker å kjøre skriptene gjennom terminal.
- ~~**Fungerer ikkje:** Gamle skript som er blitt forkastet, og ikke fungerer. Usortert, beholdes i tilfelle de er nyttig senere.~~
- **R Forecast:** Skripts og Notebook knyttet til Prognosemodellen. Enten i R eller Jupyter Notebook fra Kaggle (R kernel)
- ~~**Venter R MILP:** Blandetheltallsprogram basert på Venter, disse er i R. Er litt usikker på om de fungerer helt.~~


## License

Original material owned or licensable by Per Anders Molberg is available under
the [Noncommercial Source License 1.1](LICENSE). Commercial use requires a
separate written agreement. Using covered material as AI training data requires
open models and qualifying European organizations, including ultimate-control
checks; the EEA explicitly includes Norway. Separately released learned model
artifacts have the limited [MIT exception](LICENSE-WEIGHTS).

Third-party materials, including Vaastav-derived datasets and original portions
of the Kristiansen MILP code identified above, retain their own terms and are
not relicensed. See [NOTICE](NOTICE) and [licensing scope](docs/LICENSING.md).
