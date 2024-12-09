# Archive jobs in hrflow
`France Travail (ex: Pole Emploi)` :arrow_right: `HrFlow`

Send **archived** 'job(s)' _from_ France Travail (ex: Pole Emploi) _to_ HrFlow


**France Travail (ex: Pole Emploi) endpoint used :**
| Endpoint | Description |
| --------- | ----------- |
| [**Get all jobs**](https://api.francetravail.io/partenaire/offresdemploi/v2/offres/search) | Endpoint to search offers based on selection criteria. The list of returned offers is paginated.The range of results is limited to 150.The request method is `GET` |



## France Travail (ex: Pole Emploi) Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `client_id` :red_circle: | `string` | None | Client ID used to access France Travail (ex: Pole Emploi) API |
| `client_secret` :red_circle: | `string` | None | Client Secret used to access France Travail (ex: Pole Emploi) API |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (France Travail (ex: Pole Emploi))

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `range`  | `string\|null` | None | Pagination of data. The range of results is limited to 150. Format: p-d, where :

p is the index (starting at 0) of the first element requested, which must not exceed 3000
d is the index of the last element requested, which must not exceed 3149 |
| `sort`  | `integer\|null` | None | Sorting of data |
| `domaine`  | `string\|null` | None | Professional field codeA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint :https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//domaines |
| `codeROME`  | `string\|null` | None | ROME code of the professionA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//metiers |
| `theme`  | `string\|null` | None | Theme of the professionA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//themes |
| `appellation`  | `string\|null` | None | ROME designation code for the offer, see reference belowA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//appellations |
| `codeNAF`  | `string\|null` | None | NAF code of the offer (format 99.99X)A GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//nafs |
| `secteurActivite`  | `string\|null` | None | NAF codes for sectors of activity. It is possible to specify two NAF codes by separating them with a comma in the character string.Example : 01,02A GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//secteursActivites |
| `experience`  | `Literal['1','2','3']\|null` | None | Level of experience required
Possible values:
1 -> Less than 1 year of experience
2 -> From 1 to 3 years of experience
3 -> More than 3 years of experience |
| `experienceExigence`  | `Literal['D','E','S']\|null` | None | Filter offers by experience requirement (D beginner accepted, S experience desired, E experience required) |
| `typeContrat`  | `string\|null` | None | Contract type codeExample : CDI,CDDA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//typesContrats |
| `natureContrat`  | `string\|null` | None | Code of the nature of contractA GET request for the list of accepted choices from the Offres d'emploi API to this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//naturesContrats |
| `origineOffre`  | `Literal['1','2']\|null` | None | Origin of the offer
Possible values:
1 -> Job center
2 -> Partner |
| `qualification`  | `Literal['0','9']\|null` | None | Qualification Code
Possible values:
0 -> Non-executive
9 -> Executive |
| `tempsPlein`  | `boolean\|null` | None | Promote the use of the WeeklyDuration filter
Possible values:
false -> Part-time
true -> Full time
If the parameter is not filled, then all the offers are returned |
| `commune`  | `string\|null` | None | INSEE code of the communeA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//communes |
| `distance`  | `integer\|null` | None | Kilometric distance of the search radius
Default value: 10Note: to obtain only the offers of a specific commune, then you must fill in the parameter 'distance=0'. |
| `departement`  | `string\|null` | None | Job departmentA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//departements |
| `inclureLimitrophes`  | `boolean\|null` | None | Include bordering departments in the search |
| `region`  | `string\|null` | None | Code of the region of the offerA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//regions |
| `paysContinent`  | `string\|null` | None | Code of the country or continent of the offerA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//paysAND https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//continents |
| `niveauFormation`  | `string\|null` | None | Level of education requiredA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//niveauxFormations |
| `permis`  | `string\|null` | None | Code of the requested licenseA GET request for the list of accepted choices from the Offres d'emploi APIto this endpoint : https://api.francetravail.io/partenaire/offresdemploi/v2/referentiel//permis |
| `motsCles`  | `string\|null` | None | Search by keyword

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
| `salaireMin`  | `number\|null` | None | Minimum wage, expressed in Euro.If this data is filled in, the code of the type of minimum wage is mandatory. |
| `periodeSalaire`  | `Literal['A','C','H','M']\|null` | None | Period for the calculation of the minimum wage.
If this data is filled in, the minimum wage is mandatory.
Possible values:
M -> Monthly
A -> Annual
H -> Hourly
C -> Fee |
| `accesTravailleurHandicape`  | `boolean\|null` | None | Jobs for which the employer is disabled-friendly |
| `offresMRS`  | `boolean\|null` | None |  Allows you to search for jobs that offer the simulation recruitment method |
| `grandDomaine`  | `Literal['A','B','C','C15','D','E','F','G','H','I','J','K','L','L14','M','M13','M14','M15','M16','M17','M18','N']\|null` | None | Code of the major area of the offer |
| `experienceExige`  | `Literal['D','E','S']\|null` | None | Filter offers by experience level. |
| `publieeDepuis`  | `Literal['1','3','7','14','31']\|null` | None | Maximum number of days since the publication of the offer
Possible values: 1, 3, 7, 14, 31 |
| `minCreationDate`  | `string\|null` | None | Minimum offer creation date.
If this data is filled in, the maximum offer creation date is mandatory.
ISO-8601 standard (YYYY-MM-DDTHH:MM:SSZ) |
| `maxCreationDate`  | `string\|null` | None | Maximum offer creation date.
If this data is filled in, the minimum offer creation date is mandatory.
ISO-8601 standard (YYYY-MM-DDTHH:MM:SSZ) |
| `partenaires`  | `string\|null` | None |  This filter allows you to enter your partner code in order to include or exclude your offers from the results according to the selectionmade in the PartnerSelection mode filter
It is possible to enter several codes (separator ',').  |
| `modeSelectionPartenaires`  | `Literal['EXCLU','INCLUS']\|null` | None | Selection mode of the partner offers.

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
Note: In all cases, if originOffer = 1, then only the France Travail (ex: Pole Emploi)
offers will be returned |
| `dureeHebdo`  | `Literal['0','1','2']\|null` | None | Filtre les offres selon la durée hebdomadaire.
Valeurs possibles :
0 -> Non précisé
1 -> Temps plein
2 -> Temps partiel |
| `dureeHebdoMin`  | `integer\|null` | None | Minimum weekly duration of the offer
The value must be in HHMM format, for example : 8h => 800 ; 24h30 => 2430 |
| `dureeHebdoMax`  | `integer\|null` | None | Maximum weekly duration of the offer
The value must be in HHMM format, for example: 8h => 800; 24h30 => 2430 |
| `dureeContratMin`  | `number\|null` | None | Minimum duration of the sought contract.
The search is done in months (ex: 0.5 for 15 days, 1.0 for 1 month,2.0 for 2 months).
Positive decimal (Decimal separator: '.') |
| `dureeContratMax`  | `number\|null` | None | Maximum duration of the sought contract.
The search is made in months (ex: 0.5 for 15 days, 1.0 for 1 month,2.0 for 2 months).
Positive decimal (Decimal separator: '.') |
| `offresManqueCandidats`  | `boolean\|null` | None | Filters offers older than 15 days, with less than 4 applications (of which Pôle emploi is informed)
false -> Offers not concerned
true -> Offers with few candidates |
| `entreprisesAdaptees`  | `boolean\|null` | None | Filter the offers where the adapted company allows a disabled worker to exercise a professional activity in  conditions adapted to his capacities
false -> Offers not concerned
true -> Offers from adapted companies |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `board_key` :red_circle: | `string` | None | HrFlow.ai board key |

## Other Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `workflow_id` :red_circle: | `string` | None | A stable identifier used for persisting in incremental mode |
| `logics` :red_circle: | `array\|null` | None | A list of functions called in sequence with each item pulled from the origin. Each function might either return it's argument or None to discard the item. Any item discarded is eventually not pushed to the target |
| `format`  | `Callable\|null` | None | A formatting function to apply on items pulled before the push |
| `callback`  | `Callable\|null` | None | Registers a callback function to be called at the of a successful execution |
| `persist`  | `boolean` | True | When False has the effect of running in dry mode. Items are pulled but not pushed to the target |
| `incremental`  | `boolean` | False | Controls the incremental reading execution mode |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors.v2 import FranceTravail


logging.basicConfig(level=logging.INFO)


FranceTravail.archive_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        client_id=...,
        client_secret=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        range=...,
        sort=...,
        domaine=...,
        codeROME=...,
        theme=...,
        appellation=...,
        codeNAF=...,
        secteurActivite=...,
        experience=...,
        experienceExigence=...,
        typeContrat=...,
        natureContrat=...,
        origineOffre=...,
        qualification=...,
        tempsPlein=...,
        commune=...,
        distance=...,
        departement=...,
        inclureLimitrophes=...,
        region=...,
        paysContinent=...,
        niveauFormation=...,
        permis=...,
        motsCles=...,
        salaireMin=...,
        periodeSalaire=...,
        accesTravailleurHandicape=...,
        offresMRS=...,
        grandDomaine=...,
        experienceExige=...,
        publieeDepuis=...,
        minCreationDate=...,
        maxCreationDate=...,
        partenaires=...,
        modeSelectionPartenaires=...,
        dureeHebdo=...,
        dureeHebdoMin=...,
        dureeHebdoMax=...,
        dureeContratMin=...,
        dureeContratMax=...,
        offresManqueCandidats=...,
        entreprisesAdaptees=...,
    ),
    push_parameters=dict(
        board_key=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```