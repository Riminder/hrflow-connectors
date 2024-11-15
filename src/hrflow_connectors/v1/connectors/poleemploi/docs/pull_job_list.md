# Pull job list
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
| `format`  | `typing.Callable[[typing.Dict], typing.Dict]` | [`format_job`](../connector.py#L80) | Formatting function |
| `read_mode`  | `str` | ReadMode.sync | If 'incremental' then `read_from` of the last run is given to Origin Warehouse during read. **The actual behavior depends on implementation of read**. In 'sync' mode `read_from` is neither fetched nor given to Origin Warehouse during read. |

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `str` | None | Client ID used to access Pole Emploi API |
| `client_secret` :red_circle: | `str` | None | Client Secret used to access Pole Emploi API |
| `range`  | `str` | None | Pagination of data. The range of results is limited to 150. |
| `sort`  | `int` | None | Sorting of data |
| `domaine`  | `str` | None | Professional field codeA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint :https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//domaines |
| `codeROME`  | `str` | None | ROME code of the professionA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//metiers |
| `theme`  | `str` | None | Theme of the professionA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//themes |
| `appellation`  | `str` | None | Code of the appellationA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//appellations |
| `secteurActivite`  | `str` | None | NAF codes for sectors of activity. It is possible to specify two NAF codes by separating them with a comma in the character string.Example : 01,02A GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//secteursActivites |
| `experience`  | `str` | None | Level of experience required
Possible values:
1 -> Less than 1 year of experience
2 -> From 1 to 3 years of experience
3 -> More than 3 years of experience |
| `typeContrat`  | `str` | None | Contract type codeExample : CDI,CDDA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//typesContrats |
| `natureContrat`  | `str` | None | Code of the nature of contractA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//naturesContrats |
| `origineOffre`  | `str` | None | Origin of the offer
Possible values:
1 -> Job center
2 -> Partner |
| `qualification`  | `str` | None | Qualification Code
Possible values:
0 -> Non-executive
9 -> Executive |
| `tempsPlein`  | `bool` | None | Promote the use of the WeeklyDuration filter
Possible values:
false -> Part-time
true -> Full time
If the parameter is not filled, then all the offers are returned |
| `commune`  | `str` | None | INSEE code of the communeA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//communes |
| `distance`  | `int` | None | Kilometric distance of the search radius
Default value: 10Note: to obtain only the offers of a specific commune, then you must fill in the parameter 'distance=0'. |
| `departement`  | `str` | None | INSEE code of the departmentA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//departements |
| `inclureLimitrophes`  | `bool` | None | Include bordering departments in the search |
| `region`  | `str` | None | Code of the region of the offerA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//regions |
| `paysContinent`  | `str` | None | Code of the country or continent of the offerA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//paysAND https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//continents |
| `niveauFormation`  | `str` | None | Level of education requiredA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//niveauxFormations |
| `permis`  | `str` | None | Code of the requested licenseA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel//permis |
| `motsCles`  | `str` | None | Search by keyword

Each keyword (or expression) is at least 2 characters long and must
be separated by a comma.
The search on several keywords is processed via the logical operator "AND".
The keyword search can be used to search on :

- The title of the offer (title field in the search return)
- The ROME code (romeCode field in the search return)
- The ROME label (field romeLibelle in return for the search)
- The competences label (field competences.libelle in return of the search)
- The wording of the training fields (field formations.domaineLibelle in
return of the research)
- The wording of the permits (field permits.label in return of the search)
- The language label (field languages.label in return of the search)
- The offer description if found in the offer title and/or the ROME label
(description field in the search return)

Allowed characters: [aA-zZ]+[0-9]+[space]+[@#$%^&+./-""] |
| `salaireMin`  | `float` | None | Minimum wage, expressed in Euro.If this data is filled in, the code of the type of minimum wage is mandatory. |
| `periodeSalaire`  | `str` | None | Period for the calculation of the minimum wage.
If this data is filled in, the minimum wage is mandatory.
Possible values:
M -> Monthly
A -> Annual
H -> Hourly
C -> Fee |
| `accesTravailleurHandicape`  | `bool` | None | Allows you to search for offers for which the employer is handi friendly |
| `offresMRS`  | `bool` | None |  Allows you to search for jobs that offer the simulation recruitment method |
| `grandDomaine`  | `str` | None | Code of the major area of the offer |
| `experienceExige`  | `str` | None | Filter offers by experience level. |
| `publieeDepuis`  | `str` | None | Maximum number of days since the publication of the offer
Possible values: 1, 3, 7, 14, 31 |
| `minCreationDate`  | `str` | None | Minimum offer creation date.
If this data is filled in, the maximum offer creation date is mandatory.
ISO-8601 standard (YYYY-MM-DDTHH:MM:SSZ) |
| `maxCreationDate`  | `str` | None | Maximum offer creation date.
If this data is filled in, the minimum offer creation date is mandatory.
ISO-8601 standard (YYYY-MM-DDTHH:MM:SSZ) |
| `partenaires`  | `str` | None |  This filter allows you to enter your partner code in order to include or exclude your offers from the results according to the selectionmade in the PartnerSelection mode filter
It is possible to enter several codes (separator ',').  |
| `modeSelectionPartenaires`  | `str` | None | Selection mode of the partner offers.

This filter works with the partner criterion and is dependent on the originOffer
criterion. Possible values with the results obtained according to the two other filters:

- INCLUS(INCLUDED)
originOffer empty : Returns the PE offers and the Partners listed in the Partners
criterion
originOffer at 2 : Only the offers of the Partners listed in the Partners
criterion
- EXCLU(EXCLUDED)
originOffer empty : Return the offers of PE and Partners not listed in the Partners
criterion
originOffer at 2 : Only the offers of the Partners not listed in the Partners
criterion
Note: In all cases, if originOffer = 1, then only the Pole Emploi offers
will be returned |
| `dureeHebdo`  | `str` | None | Filtre les offres selon la durée hebdomadaire.
Valeurs possibles :
0 -> Non précisé
1 -> Temps plein
2 -> Temps partiel |
| `dureeHebdoMin`  | `int` | None | Minimum weekly duration of the offer
The value must be in HHMM format, for example : 8h => 800 ; 24h30 => 2430 |
| `dureeHebdoMax`  | `int` | None | Maximum weekly duration of the offer
The value must be in HHMM format, for example: 8h => 800; 24h30 => 2430 |
| `dureeContratMin`  | `float` | None | Minimum duration of the sought contract.
The search is done in months (ex: 0.5 for 15 days, 1.0 for 1 month,2.0 for 2 months).
Positive decimal (Decimal separator: '.') |
| `dureeContratMax`  | `float` | None | Maximum duration of the sought contract.
The search is made in months (ex: 0.5 for 15 days, 1.0 for 1 month,2.0 for 2 months).
Positive decimal (Decimal separator: '.') |
| `offresManqueCandidats`  | `bool` | None | Filters offers older than 15 days, with less than 4 applications (of which Pôle emploi is informed)
false -> Offers not concerned
true -> Offers with few candidates |
| `entreprisesAdaptees`  | `bool` | None | Filter the offers where the adapted company allows a disabled worker to exercise a professional activity in  conditions adapted to his capacities
false -> Offers not concerned
true -> Offers from adapted companies |

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
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


PoleEmploi.pull_job_list(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
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
        distance=0,
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
        experienceExige="D",
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