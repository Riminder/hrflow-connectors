
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
| `logics`  | `typing.List[typing.Callable[[typing.Dict], typing.Optional[typing.Dict]]]` | [] | List of logic functions |
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L69) | Formatting function |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `str` | None | Client ID used to access Pole Emploi API |
| `client_secret` :red_circle: | `str` | None | Client Secret used to access Pole Emploi API |
| `range`  | `str` | None |  |
| `sort`  | `int` | None |  |
| `domaine`  | `str` | None |  |
| `codeROME`  | `str` | None |  |
| `theme`  | `str` | None |  |
| `appellation`  | `str` | None |  |
| `secteurActivite`  | `str` | None |  |
| `experience`  | `str` | None |  |
| `typeContrat`  | `str` | None |  |
| `natureContrat`  | `str` | None |  |
| `origineOffre`  | `str` | None |  |
| `qualification`  | `str` | None |  |
| `tempsPlein`  | `bool` | None |  |
| `commune`  | `str` | None |  |
| `departement`  | `str` | None |  |
| `inclureLimitrophes`  | `bool` | None |  |
| `region`  | `str` | None |  |
| `paysContinent`  | `str` | None |  |
| `niveauFormation`  | `str` | None |  |
| `permis`  | `str` | None |  |
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
        domaine="A11",
        codeROME="A1303",
        theme="1",
        appellation="10263",
        secteurActivite="01",
        experience="1",
        typeContrat="CDI",
        natureContrat="E1",
        origineOffre="1",
        qualification="0",
        tempsPlein=False,
        commune="21704",
        departement="01",
        inclureLimitrophes=False,
        region="01",
        paysContinent="65",
        niveauFormation="AFS",
        permis="AM",
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