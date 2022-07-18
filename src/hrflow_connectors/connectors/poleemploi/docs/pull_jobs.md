
# Pull jobs
`Pole Emploi Jobs` :arrow_right: `HrFlow.ai Jobs`

Retrieves jobs via the ***Offres d'emploi v2*** API from the Pôle emploi website based on selection criteria set in the and send them to a ***Hrflow.ai Board***.


**Pole Emploi Jobs endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
| [**Get all jobs**](https://pole-emploi.io/data/api/offres-emploi?tabgroup-api=documentation&doc-section=api-doc-section-rechercher-par-crit%C3%A8res) | Endpoint to search offers based on selection criteria. The list of returned offers is paginated.The range of results is limited to 150.The request method is `GET` |



## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Union[typing.Dict, NoneType]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L69) | Formatting function |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `str` | None | Client ID used to access Pole Emploi API |
| `client_secret` :red_circle: | `str` | None | Client Secret used to access Pole Emploi API |
| `range`  | `str` | None |  |
| `sort`  | `int` | None |  |
| `domaine`  | `str` | None | Professional field codeA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint :https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//domaines |
| `codeROME`  | `str` | None | ROME code of the professionA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//metiers |
| `theme`  | `str` | None | Theme of the professionA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//themes |
| `appellation`  | `str` | None | Code of the appellationA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//appellations |
| `secteurActivite`  | `str` | None | NAF codes for sectors of activity. It is possible to specify two NAF codes by separating them with a comma in the character string.Example : 01,02A GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//secteursActivites |
| `experience`  | `str` | None |  |
| `typeContrat`  | `str` | None | Contract type codeExample : CDI,CDDA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//typesContrats |
| `natureContrat`  | `str` | None | Code of the nature of contractA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//naturesContrats |
| `origineOffre`  | `str` | None |  |
| `qualification`  | `str` | None |  |
| `tempsPlein`  | `bool` | None |  |
| `commune`  | `str` | None | INSEE code of the communeA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//communes |
| `departement`  | `str` | None | INSEE code of the departmentA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//departements |
| `inclureLimitrophes`  | `bool` | None |  |
| `region`  | `str` | None | Code of the region of the offerA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//regions |
| `paysContinent`  | `str` | None | Code of the country or continent of the offerA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//paysAND https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//continents |
| `niveauFormation`  | `str` | None | Level of education requiredA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//niveauxFormations |
| `permis`  | `str` | None | Code of the requested licenseA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//permis |
| `motsCles`  | `str` | None |  |
| `salaireMin`  | `float` | None |  |
| `periodeSalaire`  | `str` | None |  |
| `accesTravailleurHandicape`  | `bool` | None |  |
| `offresMRS`  | `bool` | None |  |
| `grandDomaine`  | `str` | None |  |
| `experienceExigence`  | `str` | None |  |
| `publieeDepuis`  | `str` | None |  |
| `minCreationDate`  | `str` | None |  |
| `maxCreationDate`  | `str` | None |  |
| `partenaires`  | `str` | None |  |
| `modeSelectionPartenaires`  | `str` | None |  |
| `dureeHebdo`  | `str` | None |  |
| `dureeHebdoMin`  | `int` | None |  |
| `dureeHebdoMax`  | `int` | None |  |
| `dureeContratMin`  | `float` | None |  |
| `dureeContratMax`  | `float` | None |  |
| `offresManqueCandidats`  | `bool` | None |  |
| `entreprisesAdaptees`  | `bool` | None |  |
| `distance`  | `int` | 10 |  |

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `str` | None | X-API-KEY used to access HrFlow.ai API |
| `api_user` :red_circle: | `str` | None | X-USER-EMAIL used to access HrFlow.ai API |
| `board_key` :red_circle: | `str` | None | HrFlow.ai board key |
| `sync`  | `bool` | True | When enabled only pushed jobs will remain in the board |
| `update_content`  | `bool` | False | When enabled jobs already present in the board are updated |
| `enrich_with_parsing`  | `bool` | False | When enabled jobs are enriched with HrFlow.ai parsing |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import PoleEmploi


logging.basicConfig(level=logging.INFO)


PoleEmploi.pull_jobs(
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
    ),
    origin_parameters=dict(
        client_id="your_client_id",
        client_secret="your_client_secret",
        range="your_range",
        sort=0,
        domaine="your_domaine",
        codeROME="your_codeROME",
        theme="your_theme",
        appellation="your_appellation",
        secteurActivite="your_secteurActivite",
        experience="1",
        typeContrat="your_typeContrat",
        natureContrat="your_natureContrat",
        origineOffre="1",
        qualification="0",
        tempsPlein=False,
        commune="your_commune",
        departement="your_departement",
        inclureLimitrophes=False,
        region="your_region",
        paysContinent="your_paysContinent",
        niveauFormation="your_niveauFormation",
        permis="your_permis",
        motsCles="your_motsCles",
        salaireMin=0.0,
        periodeSalaire="M",
        accesTravailleurHandicape=False,
        offresMRS=False,
        grandDomaine="A",
        experienceExigence="D",
        publieeDepuis="1",
        minCreationDate="your_minCreationDate",
        maxCreationDate="your_maxCreationDate",
        partenaires="your_partenaires",
        modeSelectionPartenaires="INCLUS",
        dureeHebdo="0",
        dureeHebdoMin=0,
        dureeHebdoMax=0,
        dureeContratMin=0.0,
        dureeContratMax=0.0,
        offresManqueCandidats=False,
        entreprisesAdaptees=False,
        distance=10,
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        sync=True,
        update_content=False,
        enrich_with_parsing=False,
    )
)
```