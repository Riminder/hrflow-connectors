# CHANGELOG



## v3.0.3 (2023-09-14)

### Fix

* fix: revert on debug changes in Github Action workflow file ([`576cc57`](https://github.com/Riminder/hrflow-connectors/commit/576cc57bd798eeefa13cd833f7681a9b93f10be6))

### Unknown

* doc: remove old commit from CHANGELOG ([`50cb4d4`](https://github.com/Riminder/hrflow-connectors/commit/50cb4d4f8226c7db920f0c20f3989b22b0aae926))


## v3.0.2 (2023-09-14)

### Fix

* fix: add missing env config in tool.semantic_release.commit_author ([`30bd071`](https://github.com/Riminder/hrflow-connectors/commit/30bd07111d7f497da724dd135dfcf80e15b3a7d9))

* fix: remove config of older version of semantic-release

This also updates the expected git author name to avoid infinite loops ([`5badff4`](https://github.com/Riminder/hrflow-connectors/commit/5badff43ab38a4297a227d5879702e33cb5535f0))

### Unknown

* debug: remove dependency from cd job ([`5ed10d6`](https://github.com/Riminder/hrflow-connectors/commit/5ed10d669d44e2dd496ecb51c9ad5a636aa8a41f))

* debug: temporarly disable core and integration jobs while debugging ([`d189416`](https://github.com/Riminder/hrflow-connectors/commit/d189416e8490be5142fe5b6eefa4421dca149f0c))

* debug: dump github event for debugging ([`8cb3214`](https://github.com/Riminder/hrflow-connectors/commit/8cb32141d68821681db1346fee5b7a350d46d12e))


## v3.0.1 (2023-09-14)

### Build

* build: setup automatic PyPI publishing using Github Actions ([`7bd1613`](https://github.com/Riminder/hrflow-connectors/commit/7bd161318b050a7b46564f6161ec0268533ecc8b))

### Chore

* chore: remove dependency for cd job on integration tests ([`3abb0b7`](https://github.com/Riminder/hrflow-connectors/commit/3abb0b74aa3e41ce3d66159d395b5df443c964d7))

### Ci

* ci: use fetch-depth: 0 to make doc generation that relies on git history work ([`1137031`](https://github.com/Riminder/hrflow-connectors/commit/11370319e599beee7dfaac9c5f3d7404276c1526))

* ci: show diff when pre-commit fail ([`4a97136`](https://github.com/Riminder/hrflow-connectors/commit/4a971361444be567abfd7f4ed0404971cda7a919))

* ci: remove commitlint to preserve team velocity at this stage ([`5d89705`](https://github.com/Riminder/hrflow-connectors/commit/5d8970594b501fc30dd203435df1098baa0d4540))

### Documentation

* docs: correct link to actoin in workable readme ([`ef8bb7b`](https://github.com/Riminder/hrflow-connectors/commit/ef8bb7bc12bb2daf6f3946c402c3d69b321be168))

* docs: fix name of workable readme file ([`eb85705`](https://github.com/Riminder/hrflow-connectors/commit/eb8570548d86d5efc848136b8f43584f6c139b8f))

* docs: update last update date for workable connector ([`7af3d33`](https://github.com/Riminder/hrflow-connectors/commit/7af3d33b9fd8fb455e25dd18734f0225b264a671))

* docs: update last update date for smartrecruiters connector ([`4b438d3`](https://github.com/Riminder/hrflow-connectors/commit/4b438d3b0903ac7d181b0d659f2eace0746c1269))

### Fix

* fix: env context is not availble in job.*.if ([`a4c1f9e`](https://github.com/Riminder/hrflow-connectors/commit/a4c1f9e3e3739b1c870c3940227d4d8b99c3ffc2))

* fix: use Personal Access Token protected by release environement to allow publish on protected master ([`ccdcacb`](https://github.com/Riminder/hrflow-connectors/commit/ccdcacbf8f2c5efe4d82886550536bbcf7d8c141))

* fix: add semantic-release version before publish ([`7bcc3fd`](https://github.com/Riminder/hrflow-connectors/commit/7bcc3fdb9cc79ed3c59f17a2440b97a2bb55c736))

* fix: setup python in cd job ([`aa66a7c`](https://github.com/Riminder/hrflow-connectors/commit/aa66a7cf71441f4dce7d5d13e7f7b04a2b1e1d8f))

* fix: load poetry venv for cd job ([`8f92b74`](https://github.com/Riminder/hrflow-connectors/commit/8f92b740b14440539df03e00d0ea8e3ce7124a8b))

* fix: add poetry to path for cd job ([`91239ac`](https://github.com/Riminder/hrflow-connectors/commit/91239acbe8e4190f67ec6bc1731c37efb3758a58))

* fix: update permission cheking for core-tests job to allow for push in master ([`4df283c`](https://github.com/Riminder/hrflow-connectors/commit/4df283c5a6166e7bc8e35a9f3077da042bf3d969))

* fix: install Angular commitlint config for commitlint GA job ([`006f45c`](https://github.com/Riminder/hrflow-connectors/commit/006f45ce9ea4f711ad6b99f5759ad5e8bebe1fbc))

* fix: correct install from Test PyPi step command ([`590a70c`](https://github.com/Riminder/hrflow-connectors/commit/590a70c11e88725b41fec43717268b85f18d7fff))

* fix: correct yaml syntax error in Github Action workflow ([`054917a`](https://github.com/Riminder/hrflow-connectors/commit/054917a076b8595bd9f556c2abfd30460ef29044))

### Style

* style: update flake8-black version to fix issue with preview option ([`e23975d`](https://github.com/Riminder/hrflow-connectors/commit/e23975db824098ef72b41e439cff1f9a4ae9e913))

* style: allow for longer header for commits ([`2bdcc47`](https://github.com/Riminder/hrflow-connectors/commit/2bdcc4784d71914d01c8ab75a6c1ef07db92db16))


## v3.0.0 (2023-09-11)

### Build

* build: Laying basis for automatic PyPI publishing ([`a7f0392`](https://github.com/Riminder/hrflow-connectors/commit/a7f0392b5743868f81359eb38f58c3d9b4458a0a))

### Documentation

* docs: wip: change structure of CONTRIBUTING ([`4d02919`](https://github.com/Riminder/hrflow-connectors/commit/4d02919ca4ac36eb86386f631b35de7c1e771398))

* docs: remove coma in Taleez doc example ([`3272a6d`](https://github.com/Riminder/hrflow-connectors/commit/3272a6dfbe4d4e036da092ed663bdb5eb03b50b6))

* docs: remove code tag in endpoint list for Monster ([`b347a42`](https://github.com/Riminder/hrflow-connectors/commit/b347a4255170c665074d9c1626937c708194dad0))

* docs: remove return between profile and `)` ([`069311e`](https://github.com/Riminder/hrflow-connectors/commit/069311ecdc5fec2e65d020cc2aaddbdda61b2459))

* docs: remove return between auth and `)` ([`fad9d32`](https://github.com/Riminder/hrflow-connectors/commit/fad9d328ec5a0e467c7e9e192000b59a5783cea0))

* docs: fix import example `form` -&gt; `from` ([`92490ee`](https://github.com/Riminder/hrflow-connectors/commit/92490eef4121cb3175700ae80dd76689213c3c4d))

* docs: remove return before `client` in example ([`9fb75e9`](https://github.com/Riminder/hrflow-connectors/commit/9fb75e9a4bf9ccae0e10409714943dbf0d74c274))

* docs: remove logger in connector example ([`63c2624`](https://github.com/Riminder/hrflow-connectors/commit/63c26240f6d81942abd67fa539a865846cad3a85))

* docs: remove space before `)` in example ([`4707f77`](https://github.com/Riminder/hrflow-connectors/commit/4707f77281f9a489529245564350099cde8d0481))

* docs: rename `Action` in DOCUMENTATION ([`a2263ce`](https://github.com/Riminder/hrflow-connectors/commit/a2263cef5ec19cf9babf872fab43dc0f0b291997))

* docs: update comment separator in `test_action.py` ([`8e91bc0`](https://github.com/Riminder/hrflow-connectors/commit/8e91bc0c81a2914fc26995ab86b8b32c775f5011))

* docs: add docs in each connector ([`7076ac4`](https://github.com/Riminder/hrflow-connectors/commit/7076ac4557a1aa0f8709a1560db304c323a6fd68))

* docs: `Ceridian` connector class ([`772044f`](https://github.com/Riminder/hrflow-connectors/commit/772044fe9a32961899150bdc22a39937d6079e58))

* docs: add link to endpoint doc for each action ([`caaa7bd`](https://github.com/Riminder/hrflow-connectors/commit/caaa7bd099090438b401fb6a7e3dc8156dc6e783))

* docs: update `About Hrflow` in project README ([`5c9c55b`](https://github.com/Riminder/hrflow-connectors/commit/5c9c55be9f2b0f7603fdf3bb051b78d0998dee59))

* docs: quote website description for each connector ([`3c06d2c`](https://github.com/Riminder/hrflow-connectors/commit/3c06d2cab0f33cf43349ce05959bfcff7370b10c))

* docs: use array instead of list to enum actions ([`2c5b760`](https://github.com/Riminder/hrflow-connectors/commit/2c5b760a2cacfeba4fbde93ad2335e88cb9d14d0))

* docs: remove comments in `Actions` ([`2f8dd0f`](https://github.com/Riminder/hrflow-connectors/commit/2f8dd0f7f33753b08e0a52bf22c8da7c5f28f698))

* docs: add basic description to `PullAction` ([`33d00c0`](https://github.com/Riminder/hrflow-connectors/commit/33d00c09a7e5d5e95aca71e71c9d53a9b86fcabd))

* docs: add description for `Action` ([`41c825f`](https://github.com/Riminder/hrflow-connectors/commit/41c825f1fdfb3b6995b808bd222339db9f5eb69b))

* docs: add basic doc for `Connector` ([`18614c9`](https://github.com/Riminder/hrflow-connectors/commit/18614c9dbe17357089286fb84e212526727f7485))

* docs: add basic description to `PullAction` ([`a1ea537`](https://github.com/Riminder/hrflow-connectors/commit/a1ea537b993c4836841cb33fac09fc3dc887b26f))

* docs: add description for `Action` ([`cc4debb`](https://github.com/Riminder/hrflow-connectors/commit/cc4debbfb9c1a92f9ee000f456376e896d4bb3e7))

* docs: add basic doc for `Connector` ([`d07dfb3`](https://github.com/Riminder/hrflow-connectors/commit/d07dfb3a33917fc8fd1a93b17c7751a8a589529d))

* docs: add connector names in README ([`4b44ffe`](https://github.com/Riminder/hrflow-connectors/commit/4b44ffe05a709414a11482a89987a0aaaf060c92))

### Feature

* feat: Update nox session that checks documentations coherence over python versions ([`b23ed98`](https://github.com/Riminder/hrflow-connectors/commit/b23ed98293151d08353f0aa1692714d18308802f))

* feat: Add update root README step during doc generation ([`39605ff`](https://github.com/Riminder/hrflow-connectors/commit/39605ff8db9d207eb079d3f4134be69172d81750))

* feat: Add new type field to ConnectorModel ([`fce9f5e`](https://github.com/Riminder/hrflow-connectors/commit/fce9f5e6a4588f307560fc902cf2e56caadfb9c0))

* feat: Add auto update of actions section in connector README ([`5cae8b2`](https://github.com/Riminder/hrflow-connectors/commit/5cae8b22bd6753ebbf28a32b273edec178ab9e18))

* feat: Add support for remote url for code links in documentation ([`9d7c69b`](https://github.com/Riminder/hrflow-connectors/commit/9d7c69bcbe452448330b137ab0c270744627e5ed))

* feat: Add test for bad connector copy cases ([`98d2b6d`](https://github.com/Riminder/hrflow-connectors/commit/98d2b6df1f68f2c22b402fed219834a8ba85f9be))

* feat: Add annotations for future to allow usage of typing.Self for python&lt;3.10 ([`5f5ba78`](https://github.com/Riminder/hrflow-connectors/commit/5f5ba7888d34771fba60001a467df3780a4ad9ec))

* feat: Labels is not consistent sometime seems to track yes / no / later stages but not always ([`a1feabf`](https://github.com/Riminder/hrflow-connectors/commit/a1feabf713871da538f91618b946ec1e6555238b))

* feat: Prune fields used for HrFlow Job SF Table ([`cdee54c`](https://github.com/Riminder/hrflow-connectors/commit/cdee54cb346a496ee57d0ff073e4e61476189e84))

* feat: Prune fields using for HrFlow Profile SF Table ([`d1855ce`](https://github.com/Riminder/hrflow-connectors/commit/d1855ce129211faee17b8b9f23617073e609defa))

* feat: Update dates for Salesforce connector ([`827f244`](https://github.com/Riminder/hrflow-connectors/commit/827f244ce61ba967c61be4f445f8c70203faf312))

* feat: Correct name for Waalaxy action and update manifest and docs ([`e2ffd73`](https://github.com/Riminder/hrflow-connectors/commit/e2ffd73b8f3f3d2b91ce34dff0c49ce3de8d14ba))

* feat: Check that pull_(job|profile)_list are only used with trigger_type=WorkflowType.pull ([`bcc550f`](https://github.com/Riminder/hrflow-connectors/commit/bcc550fa57b37bf08ed0f8a5a45cc6871711a49b))

* feat: scoring_enabled and searching_searching not required because HrFlowProfileWarehoue does not return this data ([`924b88b`](https://github.com/Riminder/hrflow-connectors/commit/924b88b5dae9cd784da48423cc30ad1cf37e1291))

* feat: Add Salesforce.push_profile action ([`4be70dd`](https://github.com/Riminder/hrflow-connectors/commit/4be70ddde95f460a69d566c3d1ccaf26cce8fb5c))

* feat: Add Salesforce pull_job_list ([`2b9e96c`](https://github.com/Riminder/hrflow-connectors/commit/2b9e96c8f109f2982b1db63e42974c45bb31fd0e))

* feat: Remove old code based on Github Secrets from Github Action workflow file ([`e20e2cc`](https://github.com/Riminder/hrflow-connectors/commit/e20e2cc05c6dcab3432d023f0e6e65d452b1a357))

* feat: Update flake8 config ([`5dc598c`](https://github.com/Riminder/hrflow-connectors/commit/5dc598cd7c3108dff54c3b8d0f7db08c71a70126))

* feat: Add tests to check that action name is correct ([`09ec6d3`](https://github.com/Riminder/hrflow-connectors/commit/09ec6d3a75d4d6cf813367e65cfbe8a07629def3))

* feat: Update action names in test-config.yaml files ([`fbd494a`](https://github.com/Riminder/hrflow-connectors/commit/fbd494a3d0cd0209a2e98bad025527882cb0e5a5))

* feat: Fix typing issues for earlier versions of Python ([`e86834a`](https://github.com/Riminder/hrflow-connectors/commit/e86834a1685eca5daf485c911b6ad715a1e22475))

* feat: Same as for action_parameters raise validation error when unexpected params are passed to (target|origin)_parameters ([`9579c4f`](https://github.com/Riminder/hrflow-connectors/commit/9579c4fe118676dac43f07ed238255ace64aa8dd))

* feat: Raise validation error when unexpected params are passed to action_parameters ([`9a10859`](https://github.com/Riminder/hrflow-connectors/commit/9a108592934a56ddfe514aad8a8bfe980a8198fb))

* feat: Add docs and update manifest for Salesforce pull_profile_list ([`ec2d9a4`](https://github.com/Riminder/hrflow-connectors/commit/ec2d9a48844fca2bacf02966f5d8267e1ef926a4))

* feat: Complete Salesforce pull_profile_list action with attachment and ome hedge cases management ([`dc2cd6e`](https://github.com/Riminder/hrflow-connectors/commit/dc2cd6e2e404610ef16aa86739df66b0dd2fb395))

* feat: Almost complete pull_profile_list action for Salesforce Connector ([`ea96b52`](https://github.com/Riminder/hrflow-connectors/commit/ea96b522b7bb744ed342854e77d0d64ba48a482c))

* feat: Salesforce ReadME with instructions about how to create Salesforce
Custom Objects ([`a19b07b`](https://github.com/Riminder/hrflow-connectors/commit/a19b07b5ea95d100811be90574c0c32c20def70d))

* feat: Update TalentSoft related manifest and docs ([`a359c73`](https://github.com/Riminder/hrflow-connectors/commit/a359c7347f5294c4cac757f1b16fc306b6182e21))

* feat: Talensoft add max_read to control number of jobs pulled ([`cc7aca5`](https://github.com/Riminder/hrflow-connectors/commit/cc7aca5d4bea17c034b5c0e927a6d490a3bac925))

* feat: Add timeout to request made to Talentsoft endpoint ([`4825e5d`](https://github.com/Riminder/hrflow-connectors/commit/4825e5d9524d18626bcef555f5583dc267ecee39))

* feat: Add Incremental support for TSJobs Warehoue ([`da9f6c2`](https://github.com/Riminder/hrflow-connectors/commit/da9f6c23eff2852adea239332eff47b81949d758))

* feat: Use enum.value explicitly for country code when querying Adzuna ([`7f030aa`](https://github.com/Riminder/hrflow-connectors/commit/7f030aaa0e313e699dc51d9a688211a4187bf5e0))

* feat: Activate Waalaxy integration tests ([`8edb324`](https://github.com/Riminder/hrflow-connectors/commit/8edb32474f29a6cb5be8ca322630c152aba7ab83))

* feat: Waalaxy trigger_view and trigger_connexion write a hardcoded profile to HrFlow. But since the reference does not change it can only fail. The tests checks that the formatting function work and that it only fails at the write step which gives some level of coverage ([`3fb6b97`](https://github.com/Riminder/hrflow-connectors/commit/3fb6b97eb38af66538f48f0a3749208254dd4d84))

* feat: Activate Hubspot and Adzuna ([`f17e6f1`](https://github.com/Riminder/hrflow-connectors/commit/f17e6f1ccb257cb8a5d1e417c30af26e23726db0))

* feat: Pass push_profile Hubspot test to failing because the same profile cannot be added multiple times ([`60cb39c`](https://github.com/Riminder/hrflow-connectors/commit/60cb39cf75b8c6e439e6060d1244ec37893f467f))

* feat: Change test params for Adzuna tests to make execution faster ([`c226900`](https://github.com/Riminder/hrflow-connectors/commit/c226900d38c538dbdfa78a63ebd96541c1660750))

* feat: Update documentation and manifest ([`b378a2e`](https://github.com/Riminder/hrflow-connectors/commit/b378a2e630a3763265df74034beade0aaa18df9d))

* feat: Remove default distance of 10 and exclude None from params ([`9a3781b`](https://github.com/Riminder/hrflow-connectors/commit/9a3781b074b3f788fbd824413003cab3741c9ef5))

* feat: Temporarly only test SmartRecruiters in CI/CD while secrets are added to AWS Secrets Manager by contributors ([`3dbd3cb`](https://github.com/Riminder/hrflow-connectors/commit/3dbd3cb8965c836ec4cc1c2d97ce00b81eaf3cff))

* feat: Handle case where file sets are different between baseline and generate in docs checking Nox session ([`898f15d`](https://github.com/Riminder/hrflow-connectors/commit/898f15d05671ab909619efe239d5d8ecbaf8366b))

* feat: Add PYTHONPATH variable to GA Workflow environment ([`1f14762`](https://github.com/Riminder/hrflow-connectors/commit/1f1476208fbdc23720afa10ecae986ee6d9d5d64))

* feat: Add Nox suite to check that documentation is consistent accross version with a few changes to doc generation ([`1e63422`](https://github.com/Riminder/hrflow-connectors/commit/1e6342251993efb29db0052c454cf7fb51d24af2))

* feat: Update pydantic version and add Nox test suite to verify consistent generation of manifest accross various versions of Python ([`49d5e01`](https://github.com/Riminder/hrflow-connectors/commit/49d5e01431dc569b200bd4ca0666d56c69f972d0))

* feat: Add test for s3 backend with implicit credentials ([`79169be`](https://github.com/Riminder/hrflow-connectors/commit/79169becaa63fc39483f7888adb0ae7c2334b2c9))

* feat: If README.md is present do nothing otherwise generate according to new template ([`1a5f67a`](https://github.com/Riminder/hrflow-connectors/commit/1a5f67a49814711eddb1ec3f99beac0167c1662e))

* feat: Allow S3 backend to be configured without explicit credentials ([`d326bfd`](https://github.com/Riminder/hrflow-connectors/commit/d326bfd88f9c9d9fb0dca612f4a74b4399792f91))

* feat: Use exception rathen than error to have stacktrace alongside error message ([`1197701`](https://github.com/Riminder/hrflow-connectors/commit/119770195a1a342aa0c189457d8688ddcbf8c332))

* feat: Update hrflow SDK version and use storing rather than searching ([`64ed93b`](https://github.com/Riminder/hrflow-connectors/commit/64ed93b6a469af0ca10b4fda2d68200c13c022cd))

* feat: Update DOCUMENTATION.md to reflect new ParametersModel and field_type constraints ([`801e697`](https://github.com/Riminder/hrflow-connectors/commit/801e697c284ce5d84ee5fe946fa2bcf588378fa3))

* feat: Update manifest.json with new field_type ([`66a1861`](https://github.com/Riminder/hrflow-connectors/commit/66a1861bd5db2409fb74458d810cd3ff11605b95))

* feat: Update existing warehouses with field_type ([`aa4539e`](https://github.com/Riminder/hrflow-connectors/commit/aa4539e477967f9686fc5a3759318075572d972e))

* feat: Update tests and add tests for field_type validation ([`a44f365`](https://github.com/Riminder/hrflow-connectors/commit/a44f36509e368d5651ae8e8c9511e5a4d6ab018c))

* feat: Add instruction for install of project with s3 extra in CONTRIBUTING.md ([`e612373`](https://github.com/Riminder/hrflow-connectors/commit/e612373aa5ff1a0561326156634f01ec5ac66181))

* feat: Fix code blocks that didn&#39;t work when s3 extra is not installed ([`f956967`](https://github.com/Riminder/hrflow-connectors/commit/f956967ed6fc5c78c1e33598a9bf998aa84fd209))

* feat: Add event for callback executed and section about callback in DOCUMENTATION.md ([`ca79549`](https://github.com/Riminder/hrflow-connectors/commit/ca79549a563ddd768b1d576d4f4255b677eb4e9b))

* feat: Exit early in action run if writing fails with unexpected error ([`b3d349c`](https://github.com/Riminder/hrflow-connectors/commit/b3d349c920ea742e46115608a3fce275b68fe411))

* feat: Add S3Store to list of backends ([`cb2d600`](https://github.com/Riminder/hrflow-connectors/commit/cb2d6004d47c519144049b7ce5061506b82fc2ba))

* feat: Refactor backend code into module ([`207e43c`](https://github.com/Riminder/hrflow-connectors/commit/207e43c1f3deff1c9c9ee9d8dea0af6437d4f769))

* feat: Add S3_STORE_TEST_* environment variables for tests ([`94debf6`](https://github.com/Riminder/hrflow-connectors/commit/94debf657a5cf0b215cdf183da9f0dd9b3c12c73))

* feat: Add s3 extra with boto3 and update poetry install step in Github action ([`097361c`](https://github.com/Riminder/hrflow-connectors/commit/097361ca9d0dc282fc85fbf38ca9c45cf1a96508))

* feat: Update DOCUMENTATION.md with instructions about incremental reading mode and few other updates ([`a9e9e04`](https://github.com/Riminder/hrflow-connectors/commit/a9e9e0465031655997bca15f1a600657dba0b030))

* feat: Add supports_incremental to manifest.json ([`d5a26da`](https://github.com/Riminder/hrflow-connectors/commit/d5a26da422020c5dbfe57f536dc1234d812e55e6))

* feat: Update manifest and docs ([`4fde4b3`](https://github.com/Riminder/hrflow-connectors/commit/4fde4b368553c94fd355690ef9eced14526d57ef))

* feat: Update existing read function with new signature that allows incremental read ([`38c2694`](https://github.com/Riminder/hrflow-connectors/commit/38c26947a979def9b58dd869dd5c5051dd0cdeaf))

* feat: Store run results in backend when configured ([`857add0`](https://github.com/Riminder/hrflow-connectors/commit/857add002f996fd1160e0cfa447c5213a75ce6d6))

* feat: Add workflow_id as argument to action run ([`8a38e0f`](https://github.com/Riminder/hrflow-connectors/commit/8a38e0f60fe09b4cfbd5f218b4ccb6640030d78a))

* feat: Refactor EventParsingError into more generic InitError ([`3e48e20`](https://github.com/Riminder/hrflow-connectors/commit/3e48e201960c3bd0462f273e36fa1bd902ea414a))

* feat: Add trigger_type to ConnectorAction ([`8996b08`](https://github.com/Riminder/hrflow-connectors/commit/8996b08dfe0ee48e64914d267d79643a25c31a5c))

* feat: Add data_type toWarehouse and ConnectorAction ([`f5bb209`](https://github.com/Riminder/hrflow-connectors/commit/f5bb20996609c8e10f151154baa4242c243dfbf0))

* feat: Update docs logic related to field types ([`1c4b149`](https://github.com/Riminder/hrflow-connectors/commit/1c4b14934b22baef0a2c40ef8ffe6a986284c074))

* feat: Use python version 3.10.5 ([`eba4b23`](https://github.com/Riminder/hrflow-connectors/commit/eba4b238a7ac6f2108a73e00bc76416067e78c32))

* feat: Handle case when nextPageId is empty string and add limit parameter to SmartRecruiters Read warehoue ([`92d3389`](https://github.com/Riminder/hrflow-connectors/commit/92d3389c1786beea3a89e9c581afd8a7cc2a899d))

* feat: Add action_by_name helper ([`72c5806`](https://github.com/Riminder/hrflow-connectors/commit/72c58061ead4a799e00627977855a31260a1e6e3))

* feat: Update docs and manifest following new pull_jobs action ([`edea8e6`](https://github.com/Riminder/hrflow-connectors/commit/edea8e6861c22dd35e6b75e19bdde5e58e2594f9))

* feat: Add pull_jobs action to TalentSoft connector ([`2bd8cbb`](https://github.com/Riminder/hrflow-connectors/commit/2bd8cbbf784b020bd69ef7441e7c6883601c0987))

* feat: Add TalentSoft jobs warehouse with read capability ([`388b1d5`](https://github.com/Riminder/hrflow-connectors/commit/388b1d5b211774a4e04cfd7ae9b6a0832c2f41d6))

* feat: Update ConnectorAction logic and manifest to work for both Catch and Pull ([`a120a77`](https://github.com/Riminder/hrflow-connectors/commit/a120a77e9f0316652d053731b2ac8294d716a36c))

* feat: Correct logics, format and event_parser in manifest.json ([`52fa62d`](https://github.com/Riminder/hrflow-connectors/commit/52fa62d2111de4398f9ce4ed61fa625ab0f1c28b))

* feat: Add new pull_profiles PULL connector action ([`3609292`](https://github.com/Riminder/hrflow-connectors/commit/36092927a254ca3c6866c8ee7cb52a1061e044b2))

* feat: Use Status.success_with_failures in case of callback_failure event ([`ea59b8a`](https://github.com/Riminder/hrflow-connectors/commit/ea59b8a5ff02b225bbdb97c4d92e6f1cd68183c1))

* feat: Only log not found message if response is empty for first request with offset=0 ([`184d94a`](https://github.com/Riminder/hrflow-connectors/commit/184d94a97db1d4b00789bf8a3e5e7a1592a55d7b))

* feat: Allow to define default event_parser for BaseActionParameters ([`84ebaa9`](https://github.com/Riminder/hrflow-connectors/commit/84ebaa997810743a1107ee8c71b0781c104dc0d8))

* feat: Update manifest and documentation with new event_parser feature ([`edaa470`](https://github.com/Riminder/hrflow-connectors/commit/edaa470d0f716828053d98f25868b8c3f9b912a1))

* feat: Add tests for workflow code generated in manifest ([`3b61f11`](https://github.com/Riminder/hrflow-connectors/commit/3b61f1107969c22e42ab7a16cada76b4a43cf632))

* feat: Add new event_parser functionnality for Catch workflows ([`3da9eb0`](https://github.com/Riminder/hrflow-connectors/commit/3da9eb0760ac78b88d27560a7067df7bab515ac3))

* feat: Update documentation ([`3001edc`](https://github.com/Riminder/hrflow-connectors/commit/3001edcc6eb2632cf3ac82b4cf7daaea7b164673))

* feat: Add callback TS reports for applicant_new and resume_update ([`757ddfb`](https://github.com/Riminder/hrflow-connectors/commit/757ddfb42a4935bc5a506a0641f16578ff25a794))

* feat: Call ConnectorAction callback with parsed origin and target parameters to allow richer scenarios ([`b54bcf2`](https://github.com/Riminder/hrflow-connectors/commit/b54bcf25ed34595215b8062dbf793436dc6c52d9))

* feat: Add TalentSoft action to handle applicant_update webhook event ([`79567a1`](https://github.com/Riminder/hrflow-connectors/commit/79567a16c7226d4ae6140924b11e90b54519e95f))

* feat: Add TalentSoft action to handle applicant_resume_update webhook event ([`2b587a4`](https://github.com/Riminder/hrflow-connectors/commit/2b587a437861c56f8d53aa848732b66ee968ba23))

* feat: Update TalentSoft read profile to use fileId information ([`7caaafc`](https://github.com/Riminder/hrflow-connectors/commit/7caaafc976cd48ab206c18e30b5919fc53a24ca9))

* feat: Update manifest and documentation to reflect new applicant_new TalentSoft action ([`2473a50`](https://github.com/Riminder/hrflow-connectors/commit/2473a508a7e944b705b59d1ad7c9dd5d86e6a08d))

* feat: Remove const field from action documentation ([`fdc2a34`](https://github.com/Riminder/hrflow-connectors/commit/fdc2a34c7635d5ea5acc99be419f3a9191deb780))

* feat: Add action to handle applicant_new TalentSoft webhook event ([`1e5e4ad`](https://github.com/Riminder/hrflow-connectors/commit/1e5e4ad5c248e9ebaa83c23cc63762942a64d991))

* feat: Rename reference to applicantId to match with TS webhook payload ([`26bd560`](https://github.com/Riminder/hrflow-connectors/commit/26bd5601b23621a0c7f3aef7b093312b2b6906ad))

* feat: Add method to fix some parameters of read/write functions ([`21390e4`](https://github.com/Riminder/hrflow-connectors/commit/21390e459291815cf4ac61ba1992d6d1fee72cf9))

* feat: Add new write Profile and write Profile parsing capabilities for HrFlow warehoues ([`6539eeb`](https://github.com/Riminder/hrflow-connectors/commit/6539eeb3f8ff6816072f4a426ebb898597cd888c))

* feat: Add Talentsoft Profile warehouse with read capability ([`ca99c85`](https://github.com/Riminder/hrflow-connectors/commit/ca99c85b0bb4d98f612c66caa22fb7fab33eff66))

* feat: Add callback functionnality to ConnectorAction ([`12ec67b`](https://github.com/Riminder/hrflow-connectors/commit/12ec67bf10a8778ef1a53f86de018a3540897af8))

* feat: Add origin and target prefix to workflow_code to avoid name collissions ([`525fa88`](https://github.com/Riminder/hrflow-connectors/commit/525fa88c3ce17d2d8720a2f57a642729555dab99))

* feat: Update testing section of DOCUMENTATION.md with new ActionRunResult format ([`387c60e`](https://github.com/Riminder/hrflow-connectors/commit/387c60e2ef50933b4784b585b2a17ba90d0e7ccd))

* feat: Add section about ActionRunResult ([`5f52c01`](https://github.com/Riminder/hrflow-connectors/commit/5f52c01a2a2862d083466a02bd74522a8f86d04d))

* feat: Update DOCUMENTATION.md with new write function signature ([`0e884ff`](https://github.com/Riminder/hrflow-connectors/commit/0e884ffb9232d949d74d9623ec98a30d9dc6d24c))

* feat: Correct typing for write Warehoue function ([`a4a017c`](https://github.com/Riminder/hrflow-connectors/commit/a4a017ca6e374e4d1353d1aeae89a9086ac652bf))

* feat: Add scenario to test success with failures mode ([`f5d992f`](https://github.com/Riminder/hrflow-connectors/commit/f5d992f42cc78a9c3419ea1eb3963f8202d053bc))

* feat: Update SmartRecruiters and HrFlow to new action flow ([`26cc318`](https://github.com/Riminder/hrflow-connectors/commit/26cc3182c404cba93f5038583b4ab7a26ed62cac))

* feat: Update tests logic to match with new ActionRunResult flow ([`c37b479`](https://github.com/Riminder/hrflow-connectors/commit/c37b479a813704dea0dbf343734a3ecffeabae25))

* feat: Update run pipeline to be more fault tolerant ([`47854f9`](https://github.com/Riminder/hrflow-connectors/commit/47854f9c9ca99d6006f24522fb6867bd3ba697cc))

* feat: Move is_readable/writable validation logic out of run main function ([`87bcb02`](https://github.com/Riminder/hrflow-connectors/commit/87bcb027bf7f53b4b8aa3417a4904cb89e857736))

* feat: Enable lfs and update actions/checkout to v3 ([`ec7493a`](https://github.com/Riminder/hrflow-connectors/commit/ec7493ae37b5b1e2518fc97b6f31c0e08a316f0e))

* feat: Use correct version of Python in Github Actions ([`52c6429`](https://github.com/Riminder/hrflow-connectors/commit/52c6429e181762f0ff422c495ccbfddb7c331f86))

* feat: Allow CONTRIBUTOR to run pipeline ([`c9c22ce`](https://github.com/Riminder/hrflow-connectors/commit/c9c22ce23837d5e89cd539bfcf07955f4ba92db5))

* feat: Add CONTRIBUTING.md file ([`98fc1eb`](https://github.com/Riminder/hrflow-connectors/commit/98fc1ebc848d07fb3df59e0488ac46a24d73ff8b))

* feat: Update DOCUMENTATION.md with instructions about how to test connectors ([`1103c6f`](https://github.com/Riminder/hrflow-connectors/commit/1103c6f5d4c84dec655e4c3fc13e37ea4882a514))

* feat: Setup utility hooks and setup git lfs for data in tests/data ([`ee9d488`](https://github.com/Riminder/hrflow-connectors/commit/ee9d488b8303801da1ac24dc8cb3c5c54862e364))

* feat: Use Black preview feature rathern than deprecated experimental-string-processing option ([`effabb6`](https://github.com/Riminder/hrflow-connectors/commit/effabb64e8836ff8c515d9fcf7c96363eaa1f51c))

* feat: Make sure in Warehouse tests that at least one element is returned if expected_number_of_items is not returned ([`fe24be8`](https://github.com/Riminder/hrflow-connectors/commit/fe24be8e1bbee2597b1b9561cf133092131a5c79))

* feat: Use 3.8.0 as minimal python version ([`5e5e409`](https://github.com/Riminder/hrflow-connectors/commit/5e5e409d89e8abf3e35e0dc1d21c41e5dee50b48))

* feat: Add AUTHORS file ([`f01566e`](https://github.com/Riminder/hrflow-connectors/commit/f01566eed91bf80f3b28643f57b2969c9b2eba7d))

* feat: Add DOCUMENTATION.md ([`fe3ce2f`](https://github.com/Riminder/hrflow-connectors/commit/fe3ce2f5d343e98bb98fff66c60dae98b9244f74))

* feat: Make data_schema optional for Warehouse ([`bc6a407`](https://github.com/Riminder/hrflow-connectors/commit/bc6a4077e24f24ddd4c9d35ad314df8cadc01634))

* feat: Add README ([`1cdc2e0`](https://github.com/Riminder/hrflow-connectors/commit/1cdc2e0259060215e22dda750c2268f81346519c))

* feat: Add shared secrets file capability to testing suite ([`bedd4a4`](https://github.com/Riminder/hrflow-connectors/commit/bedd4a4d6f7f6e095a51e50fd67a9b95b1791d64))

* feat: Add github test workflow for PRs targeting v2 ([`bec2c98`](https://github.com/Riminder/hrflow-connectors/commit/bec2c981a8a78db696ec982e77d85e0d554cd832))

* feat: Protect code in ./github with CODEOWNERS file ([`ec45cb0`](https://github.com/Riminder/hrflow-connectors/commit/ec45cb05aa9aa8c17fe1a80e2847204dbf6dc30c))

* feat: Add github test workflow for PRs targeting master ([`84ad47a`](https://github.com/Riminder/hrflow-connectors/commit/84ad47a3c438443509d6331bdd30e60e1df66ab6))

* feat: Ignore line in coverage ([`ed6de84`](https://github.com/Riminder/hrflow-connectors/commit/ed6de84331b86d1d79ddfd77393e434715413214))

* feat: Name change source -&gt; origin, destination -&gt; target ([`57707f4`](https://github.com/Riminder/hrflow-connectors/commit/57707f41f5b34e8460c57160c22a28258f5185b5))

* feat: Add test config for Smartrecruiters ([`dd9cc0f`](https://github.com/Riminder/hrflow-connectors/commit/dd9cc0f9b8a5346e230948812622ea0209579fbc))

* feat: Setup test suite logic for connectors ([`211bfe5`](https://github.com/Riminder/hrflow-connectors/commit/211bfe5f35073c051c019dd7264daa5f22760c0a))

* feat: Update pytest and pytest-cov versions ([`8a0c36b`](https://github.com/Riminder/hrflow-connectors/commit/8a0c36beb2792eb99788c5a3d1fc794254928dd0))

* feat: Init tests for module and commit core tests ([`121f3d9`](https://github.com/Riminder/hrflow-connectors/commit/121f3d90eaf7a628086a34f89ddf7f2b986ed7cb))

* feat: Minor changes ([`1b31ced`](https://github.com/Riminder/hrflow-connectors/commit/1b31ced16b98c44654cfe54d70f5b8626d1c015b))

* feat: Use more precise ActionStatus states ([`50af59a`](https://github.com/Riminder/hrflow-connectors/commit/50af59a44498fe68b02b60fa806ee2332246b48e))

* feat: Configure stages for pre-commit ([`cc31a89`](https://github.com/Riminder/hrflow-connectors/commit/cc31a89969699756f5f5d19c2bc440438f26e4ef))

* feat: Add README for SmartRecruiters connector ([`ab0b2d9`](https://github.com/Riminder/hrflow-connectors/commit/ab0b2d904fa3f6ee0f1a75037e093279d656d3be))

* feat: change logics so they return a modified object or none instead of a bool ([`6522833`](https://github.com/Riminder/hrflow-connectors/commit/6522833d2cde89c7585f3430ad1561616f49a698))

* feat: add hrflow_client as a fixed parameter in connector methods ([`6cf513e`](https://github.com/Riminder/hrflow-connectors/commit/6cf513e70c9c9167536a51649506ec72f99b1fa5))

* feat: update docstrings of connectors.py ([`7882308`](https://github.com/Riminder/hrflow-connectors/commit/7882308fcc3943690dfbc82cc39e9e89a95db74f))

* feat: add workable push profile ([`cc6ebc2`](https://github.com/Riminder/hrflow-connectors/commit/cc6ebc2c6b71b9bdb1d56e32368387bd20fd1493))

* feat: update auth in connector ([`053d460`](https://github.com/Riminder/hrflow-connectors/commit/053d4606640fd0c38960b586222e0ce10d8e0ac5))

* feat: adapt workable pull jobs connector to private endpoints ([`ab1330e`](https://github.com/Riminder/hrflow-connectors/commit/ab1330eeea4fc147af9d6c63fcff4d57ff96e7b7))

* feat: use `Config` for Teamtailor ([`994ba91`](https://github.com/Riminder/hrflow-connectors/commit/994ba916d31f63fed4df962b9ee32835679e8ed5))

* feat: use `Config` for Taleez ([`78b19da`](https://github.com/Riminder/hrflow-connectors/commit/78b19da0133334bb4755716e1c206e03aaaa8bd7))

* feat: use `Config` for SmartRecruiters ([`bfb98cc`](https://github.com/Riminder/hrflow-connectors/commit/bfb98ccee3225b513ad9f1c5fa3f1d1450a7ac02))

* feat: use `Config` for SAP ([`3dd8a50`](https://github.com/Riminder/hrflow-connectors/commit/3dd8a5079a69ca1b856fa0603642028fcd0c143d))

* feat: use `Config` for Recruitee ([`53aa6e2`](https://github.com/Riminder/hrflow-connectors/commit/53aa6e2cecd45c352bbe28de817b1ff7ca71bd5f))

* feat: use `Config` for Monster ([`c8c7ab2`](https://github.com/Riminder/hrflow-connectors/commit/c8c7ab2a4dbd7b46058292dcdfddba15f380fde4))

* feat: use `Config` for Greenhouse ([`5445b68`](https://github.com/Riminder/hrflow-connectors/commit/5445b68842fe581bec0e0ef4e5a61fd29ffb833c))

* feat: use `Config` for Flatchr ([`d0b2fa9`](https://github.com/Riminder/hrflow-connectors/commit/d0b2fa9baf473f012a9f9fe6e725c95d37ec9e8b))

* feat: use `Config` for Crosstalent ([`2400d0f`](https://github.com/Riminder/hrflow-connectors/commit/2400d0f82a47d2e19ebcaa46ff1c2def046eb749))

* feat: use `Config` for Bullhorn ([`5e67b43`](https://github.com/Riminder/hrflow-connectors/commit/5e67b43f8d4f4fef923f3ee21e7f8172e91292b6))

* feat: replace `credentials` by `Config` for Breezy ([`de9b811`](https://github.com/Riminder/hrflow-connectors/commit/de9b8113deddb5e6cf93cd89748da37abcb01244))

* feat: `Config` to get field like credentials ([`cd975a9`](https://github.com/Riminder/hrflow-connectors/commit/cd975a98dec94a22755713a3a85558e52b6649f2))

* feat: add job schemas in push job actions ([`181ce68`](https://github.com/Riminder/hrflow-connectors/commit/181ce6834abeab1a31fcff051e19af9f03d49def))

* feat: add profile schemas in push profile action for taleez ([`17d7ba5`](https://github.com/Riminder/hrflow-connectors/commit/17d7ba55737c3c2ea47e3e41d3764a73f7df6bfe))

* feat: add profile schemas in smartr ([`ba06b20`](https://github.com/Riminder/hrflow-connectors/commit/ba06b20276eb1f67e14172cad1dd01f9cf9a41c7))

* feat: add profile objects in actions ([`c3b0505`](https://github.com/Riminder/hrflow-connectors/commit/c3b05050025d150fd0b5a501bfbe1170b596d7e9))

* feat: add profile schemas in greenhouse ([`de2b540`](https://github.com/Riminder/hrflow-connectors/commit/de2b540387902d467054fcceade2bce01cdeed56))

* feat: add profile schemas in crta push action ([`1a68c43`](https://github.com/Riminder/hrflow-connectors/commit/1a68c436fb519a86ccd9b5f47d302a404e2adc82))

* feat: add profile schemas in breezy hr actions ([`843ccc6`](https://github.com/Riminder/hrflow-connectors/commit/843ccc635559277321241f33478d628437d04bac))

* feat: unit tests ([`be856d1`](https://github.com/Riminder/hrflow-connectors/commit/be856d1540b0493f754fdfb78f597d8f10459195))

* feat: add HrflowProfile and HrflowJob in Push actions ([`c1fd2c1`](https://github.com/Riminder/hrflow-connectors/commit/c1fd2c1d29a0a93cff66ed4c547ed63cede247a7))

* feat: add board and source in hrflow schemas in utils ([`fbdb3f4`](https://github.com/Riminder/hrflow-connectors/commit/fbdb3f44f40261038ad40649b99adcc72add25af))

* feat: add schemas in xml actions ([`764253c`](https://github.com/Riminder/hrflow-connectors/commit/764253c1b841b1500b3d11526c4139f019d79685))

* feat: add schemas in taleez actions ([`41bf13f`](https://github.com/Riminder/hrflow-connectors/commit/41bf13f82c1b2bd03a57e0cc12a6fde18939f7ad))

* feat: add schemas in smartrecruiters actions ([`914792a`](https://github.com/Riminder/hrflow-connectors/commit/914792a981be091b99484c448bdd7ef9cef8c896))

* feat: add schemas in sap actions ([`3db10bb`](https://github.com/Riminder/hrflow-connectors/commit/3db10bb49d0b61bdc2a8593783dd132a7dfeb819))

* feat: add schemas in recruitee actions ([`044bd2b`](https://github.com/Riminder/hrflow-connectors/commit/044bd2bcd435bc281229f46f297b8f15d6bf9ae8))

* feat: add jobs schema model in greenhouse ([`2ad5c42`](https://github.com/Riminder/hrflow-connectors/commit/2ad5c421d8f276d961a89cf0c3e5b1deb4fbacfb))

* feat(debugging): update hrflow job schemas in metadatas and other fields ([`c4303cf`](https://github.com/Riminder/hrflow-connectors/commit/c4303cf982455dad7b8fb7868c5dcc707d9c208f))

* feat: add schemas model in ceridian actions ([`3be9af4`](https://github.com/Riminder/hrflow-connectors/commit/3be9af41b5c563ef788a628688e954233b57fa1e))

* feat: add objects modles in breezyhr actions ([`f7b2807`](https://github.com/Riminder/hrflow-connectors/commit/f7b2807c57b4ca4f61e49695ae72bd90768c100d))

* feat: add Job basemodel in Talentdatatype ([`8858b11`](https://github.com/Riminder/hrflow-connectors/commit/8858b1162f29f7af65c95649a7e25bc4d91980d2))

* feat: add Hrflow objects basemodels in core/action functions args ([`cdffa6c`](https://github.com/Riminder/hrflow-connectors/commit/cdffa6cfce3a316593d3110efca80d6a40d83572))

* feat: add schemas for crosstalent push profile model ([`2896e73`](https://github.com/Riminder/hrflow-connectors/commit/2896e73399d006867191b8e8e6c45f8dbcbd1d9b))

* feat: add schema model for crosstalent jobs ([`0e93a07`](https://github.com/Riminder/hrflow-connectors/commit/0e93a07f7169ead1724d06b341bb8c17593b728c))

* feat: add hrflow profile object model ([`c32e755`](https://github.com/Riminder/hrflow-connectors/commit/c32e75581d4c898559a217dd020788e2841c2409))

* feat: add hrflow job schemas model in utils ([`934ffe4`](https://github.com/Riminder/hrflow-connectors/commit/934ffe483fb805a92e25db474129c04d3112ad02))

* feat: update push in breezy ([`1c7ffeb`](https://github.com/Riminder/hrflow-connectors/commit/1c7ffeb51a594c317cfbbade608fadc3765530c3))

* feat: add new teamtailor connector in project README ([`af506c2`](https://github.com/Riminder/hrflow-connectors/commit/af506c2b6fa73a0b1be482fe3cd31c8a00235cb0))

* feat: add schema model for ech action ([`1c2fdd6`](https://github.com/Riminder/hrflow-connectors/commit/1c2fdd6c58311afd6e6ca79c24c832240b11b5d0))

* feat: add profile connector for teamtailor ([`05c390d`](https://github.com/Riminder/hrflow-connectors/commit/05c390dd5922bb3c6fdf041943c1c098b0f33bd5))

* feat: add teamtailor jobs connector ([`01f8667`](https://github.com/Riminder/hrflow-connectors/commit/01f866785e2a8855bd8ebb86863213d135ef2e27))

* feat: use custome Error in all package ([`9d9e175`](https://github.com/Riminder/hrflow-connectors/commit/9d9e175b334940af19f35b43e460189c388074fd))

* feat: add `HrflowError` Exception ([`6d9820b`](https://github.com/Riminder/hrflow-connectors/commit/6d9820b6dd3f7e0764f1f42e3c673e733a245128))

* feat: add `PushError` &amp; `PullError` Exception ([`0fb951a`](https://github.com/Riminder/hrflow-connectors/commit/0fb951af09a7a994018d1c4d398855ee8b18b427))

* feat: update VERSION -&gt; `1.0.2` ([`d3b0cd6`](https://github.com/Riminder/hrflow-connectors/commit/d3b0cd61322b9d8a2276393a871234f201d3abe6))

* feat: Contributing guide to the project ([`9b09294`](https://github.com/Riminder/hrflow-connectors/commit/9b0929439620bcb9dc7fb710d4d8a80a667912e8))

* feat: add changelog for `1.0.0` ([`1f4bccf`](https://github.com/Riminder/hrflow-connectors/commit/1f4bccfbdb98327836b66f85c1dcde01de4fdd9d))

* feat: update VERSION -&gt; `1.0.0` ([`3e364ac`](https://github.com/Riminder/hrflow-connectors/commit/3e364acb16b6666e558ee7a0dca54a3a9cac8398))

* feat: `Breezy.hr` &amp; `XML` in connector list ([`203ec81`](https://github.com/Riminder/hrflow-connectors/commit/203ec81cdacd4038d1014bd911d7370b3cf4f465))

* feat: refacto ceridian docs ([`f17b095`](https://github.com/Riminder/hrflow-connectors/commit/f17b09521c995224616e8e70650beb933b1910f1))

* feat: add auth session test ([`2952832`](https://github.com/Riminder/hrflow-connectors/commit/29528328ece4beea800551577a62a653d72262e2))

* feat: remove `dateutil.parser` ([`4e36508`](https://github.com/Riminder/hrflow-connectors/commit/4e365087d1752d7c9f2c5c1660a16df6e3b924d8))

* feat: replace dateutil.parser with`str_to_datetime` ([`9a86046`](https://github.com/Riminder/hrflow-connectors/commit/9a86046baa1d5e90f6ec5b501c8b7ab9644426ad))

* feat: add actions enrich bullhorn ([`10471b5`](https://github.com/Riminder/hrflow-connectors/commit/10471b5ec64888154eea610b34648615cb80dd73))

* feat: add schemas model for each action object ([`e0371fe`](https://github.com/Riminder/hrflow-connectors/commit/e0371fed3ee1cc9f5eba8e647e4f0242e4e3ac62))

* feat: add documentation for each action ([`d303195`](https://github.com/Riminder/hrflow-connectors/commit/d303195668fa2fce255e9ebe34fe43a6f7b62b71))

* feat: add push profile ([`1c35974`](https://github.com/Riminder/hrflow-connectors/commit/1c35974130259a6f5e338ce9e0640df7a5194945))

* feat: add push_profile test bullhorn ([`eb657a6`](https://github.com/Riminder/hrflow-connectors/commit/eb657a6f25dc8006538ea589f33577b9e02b80ed))

* feat: add push_profile bullhorn ([`b363a76`](https://github.com/Riminder/hrflow-connectors/commit/b363a76029cc845bfb08eb8519af2d9277705fe3))

* feat: add PushProfileAction bullhorn education enrichment ([`50556b2`](https://github.com/Riminder/hrflow-connectors/commit/50556b237244ca1386e2baa2417da24747fd6dac))

* feat: add PushProfileAction bullhorn create profile ([`83c4c6d`](https://github.com/Riminder/hrflow-connectors/commit/83c4c6d090451c794035a990035cd2fbb10ff4d6))

* feat: add schemas bullhorn ([`eb1f298`](https://github.com/Riminder/hrflow-connectors/commit/eb1f2983e864179982b0a71f470ce1595956cffc))

* feat: add bullhorn init ([`2aa66b5`](https://github.com/Riminder/hrflow-connectors/commit/2aa66b59397219b968286e20e1076baec42b9d54))

* feat: add __init__ bullhorn ([`00c55bc`](https://github.com/Riminder/hrflow-connectors/commit/00c55bcfa1b8a11362597b3d0f31be877f0709b7))

* feat: add OAuth2Session ([`afd7a69`](https://github.com/Riminder/hrflow-connectors/commit/afd7a69daf021ad81d61b98c4efef00beeffe582))

* feat: add breezyhr pull jobs ([`6eceab8`](https://github.com/Riminder/hrflow-connectors/commit/6eceab8a57a3a2fc8b3c83b62304043fc9674629))

* feat: add test action CatchProfileAction ([`6009520`](https://github.com/Riminder/hrflow-connectors/commit/60095204298163aa3135359e8f7f5705982b3ea8))

* feat: add test action ([`4036c40`](https://github.com/Riminder/hrflow-connectors/commit/4036c40dc4ad8db0c9ac8366ff6f605458eabe3d))

* feat: add format ([`73312fe`](https://github.com/Riminder/hrflow-connectors/commit/73312fe0321f61416c4803fdbd0e87094d908a36))

* feat: add a new auth class for Taleez API Key ([`f390a24`](https://github.com/Riminder/hrflow-connectors/commit/f390a24b4fc05a0819ac5567854ce384ce97ca8b))

* feat: add test monster MonsterBodyAuth ([`adb4a8a`](https://github.com/Riminder/hrflow-connectors/commit/adb4a8a6e94e2a44b2fc0b8d7e51a2922dd51048))

* feat: breezy connector ([`797e338`](https://github.com/Riminder/hrflow-connectors/commit/797e338c0438d5b2a321c52b30686910b3ee3345))

* feat: add brezzy hr connecotr ([`5cab457`](https://github.com/Riminder/hrflow-connectors/commit/5cab45709a7a2b35f0826bbafe9d33fb644e0589))

* feat: add monster push_job test ([`c21406b`](https://github.com/Riminder/hrflow-connectors/commit/c21406b23c048d63d6bb6177599cc0bf79678436))

* feat: add auth monster MonsterBodyAuth ([`8d0d3b3`](https://github.com/Riminder/hrflow-connectors/commit/8d0d3b30c288a78b4ff4f446cfa097f0a02e9c90))

* feat: add monster push_job method ([`b2842e9`](https://github.com/Riminder/hrflow-connectors/commit/b2842e97ef77741a4928a1e465b9e79c41a882e1))

* feat: add monster PushJobAction ([`1191e52`](https://github.com/Riminder/hrflow-connectors/commit/1191e528da3cef13bea81067faca94e658c5413a))

* feat: add dattime format in format func ([`27a5009`](https://github.com/Riminder/hrflow-connectors/commit/27a5009ca8f73bc52d347e55feb4dad7b491c4eb))

* feat: add resume to profile urls ([`39d112d`](https://github.com/Riminder/hrflow-connectors/commit/39d112d6cb848910d0f151297153af80151fdd78))

* feat: update schemas ([`8b9e119`](https://github.com/Riminder/hrflow-connectors/commit/8b9e1196033e30eff0a4eb4df85829c5665a058a))

* feat: add schemas for each action model ([`e978a0f`](https://github.com/Riminder/hrflow-connectors/commit/e978a0ff39bd93c766f0d3f7f5f920d32a5a09f1))

* feat: add push profile to Taleez ([`e100b23`](https://github.com/Riminder/hrflow-connectors/commit/e100b23e0423a290c4f8b7caf240beccec211bd5))

* feat: add test monster catch profile ([`ba5be6b`](https://github.com/Riminder/hrflow-connectors/commit/ba5be6b557e478141a311d07bc49414011924292))

* feat: add catch_profile method ([`08ece01`](https://github.com/Riminder/hrflow-connectors/commit/08ece017219578dc87e38daded11b0308abb2039))

* feat: add monster profile __init__ ([`0524f8c`](https://github.com/Riminder/hrflow-connectors/commit/0524f8cb605d631f09178fc2f4081dea8fec1669))

* feat: add Monster connector with catch_profile method ([`fb4c742`](https://github.com/Riminder/hrflow-connectors/commit/fb4c742bcbaa483365632ce191f935953b96c9a9))

* feat: add CatchProfileAction monster action ([`c7139ed`](https://github.com/Riminder/hrflow-connectors/commit/c7139ed88a1c1a391708eedd82c1bce6104c7a4f))

* feat: add taleez `pull_jobs` action ([`9187038`](https://github.com/Riminder/hrflow-connectors/commit/9187038bb2949c77f25bffeab1f54589a1f194b4))

* feat: add seconds to isoformat converter ([`3dac395`](https://github.com/Riminder/hrflow-connectors/commit/3dac395cb46bd45b810166e72a373ed5334a99e5))

* feat: update to version 1.0.0 ([`670213e`](https://github.com/Riminder/hrflow-connectors/commit/670213e3e181ca8c821bedb6d6e00970f8c8dc28))

* feat: update schemas ([`edeadf8`](https://github.com/Riminder/hrflow-connectors/commit/edeadf81efaa1947920ad52a63a7f410a1bd4f99))

* feat: test the new adapted version ([`012822b`](https://github.com/Riminder/hrflow-connectors/commit/012822bbe55902db166fd161d6ee792f971ae4e1))

* feat: adapt project README &amp; change list to array ([`b136009`](https://github.com/Riminder/hrflow-connectors/commit/b136009e9d2889979e3ac98029da06b1800b46b1))

* feat: add schemas models for connectors ([`51dc691`](https://github.com/Riminder/hrflow-connectors/commit/51dc691b85095ad6b6e16b5514799afd18dd0b0f))

* feat: update Flatchr to architecture 1.0.0 ([`4cb2f09`](https://github.com/Riminder/hrflow-connectors/commit/4cb2f098f76b2f1026a28f427b13eaa09c8f1db9))

* feat: add test test_get_lat_lng_not_api_key_here ([`9e65d2d`](https://github.com/Riminder/hrflow-connectors/commit/9e65d2dc47b94adaac77756cba5a1af1c7673699))

* feat: add data directory on setup.cfg ([`621a4cd`](https://github.com/Riminder/hrflow-connectors/commit/621a4cd63fdf69cbd3e85ac1f19a8d10c26543fa))

* feat: add schemas.py basemodel for job json ([`55da20a`](https://github.com/Riminder/hrflow-connectors/commit/55da20acba4e2a91c5186505733b38fca255da44))

* feat: add ceridian dayforce jobs connector ([`cf9188d`](https://github.com/Riminder/hrflow-connectors/commit/cf9188df7f53a5060f28b401642d1d5371473b7a))

* feat: update XML Connector to architecture 1.0.0 ([`f89b0b9`](https://github.com/Riminder/hrflow-connectors/commit/f89b0b90d6e13ce4f499f9f8954cb220afec3703))

* feat: add feature permitting to not call here ([`9c6b03e`](https://github.com/Riminder/hrflow-connectors/commit/9c6b03e95c8aa5f1ab72369cc2156ffc309187c6))

* feat: add last 1% coverage adress not found by here ([`8d300c9`](https://github.com/Riminder/hrflow-connectors/commit/8d300c976e798f32374cd0212daaae32f09f084c))

* feat: update Greenhouse to architecture 1.0.0 ([`ede73ba`](https://github.com/Riminder/hrflow-connectors/commit/ede73ba86eb2f7fbd4737d1a66eca4c703d0e128))

* feat: add test suite get_lat_long utils ([`fea0db9`](https://github.com/Riminder/hrflow-connectors/commit/fea0db98f6d3baf893a9b15b97a3e828cd07643b))

* feat: add schemas ([`42bc04e`](https://github.com/Riminder/hrflow-connectors/commit/42bc04e786ed71a0c4ada254f5b21ca70112fc21))

* feat: add Recruitee profile connector ([`d6de22d`](https://github.com/Riminder/hrflow-connectors/commit/d6de22da523e4313f9bac0ee19d729910a60d0c5))

* feat: add schemas.py ([`081497a`](https://github.com/Riminder/hrflow-connectors/commit/081497a8ad5a1268858afc11d5b703bdf4639448))

* feat: change to archi 1.0.0 for SmartRecruiters ([`fd2e787`](https://github.com/Riminder/hrflow-connectors/commit/fd2e787645acab44a23603914ed8b179cecb8f93))

* feat: add recruitee `PullJobs` action ([`f72c7bd`](https://github.com/Riminder/hrflow-connectors/commit/f72c7bd8aaf7ad2c463e76ad045631d561a39e58))

* feat: add unit tests ([`9649f5b`](https://github.com/Riminder/hrflow-connectors/commit/9649f5b7ade4af579f655e20d7004d94d9bc88d0))

* feat: add utils get_lat_long ([`6ad70db`](https://github.com/Riminder/hrflow-connectors/commit/6ad70db5b02b9a5e740d3308e1ff77cde2790ce7))

* feat: change to architecture 1.0.0 for Crosstalent ([`8d44305`](https://github.com/Riminder/hrflow-connectors/commit/8d443050c35d32173298dbabb751a6a1823c42a2))

* feat: update schemas ([`c0e8e4b`](https://github.com/Riminder/hrflow-connectors/commit/c0e8e4bc56c00da4345fe929a8ce82d4a7e28173))

* feat: add README.md ([`55a64a3`](https://github.com/Riminder/hrflow-connectors/commit/55a64a342e29564b5cf3dde67712962f7a4267fa))

* feat: update `schemas` ([`0ac7391`](https://github.com/Riminder/hrflow-connectors/commit/0ac73913b8714e5c48db7e195a1787a63e5bab15))

* feat: add schemas ([`685d338`](https://github.com/Riminder/hrflow-connectors/commit/685d3388a8d5d37d616c0cb409f4e2170e3b8eba))

* feat: `html.unescape` in `hydrate_with_parsing` ([`71620d6`](https://github.com/Riminder/hrflow-connectors/commit/71620d6184b4744fd933b77ebe64b113200da604))

* feat: add monster connector ([`4a2508c`](https://github.com/Riminder/hrflow-connectors/commit/4a2508c075742b9b5cf6c41df3cc215228d1ef57))

* feat: add new connectors on `README.md` ([`bd855a0`](https://github.com/Riminder/hrflow-connectors/commit/bd855a0583d990f1d3da614861d3ec48423317aa))

* feat: update `schemas` ([`5d35af2`](https://github.com/Riminder/hrflow-connectors/commit/5d35af2b7cddd7fab80980a1696923c34dd9c1a0))

* feat: add schemas ([`13430bc`](https://github.com/Riminder/hrflow-connectors/commit/13430bccf61ee223a5ca104dace5252b7eab205a))

* feat: add `README.md` ([`27d6a9d`](https://github.com/Riminder/hrflow-connectors/commit/27d6a9d8912d36de7cd7428d8188f3adea7a00a2))

* feat: enrich `format` profile ([`e25117e`](https://github.com/Riminder/hrflow-connectors/commit/e25117e87c9be7b79a6edb8021db8c905ccde4e6))

* feat: remove HTTPStream &amp; replace ([`38c94a0`](https://github.com/Riminder/hrflow-connectors/commit/38c94a0e6c2b9c07b0ee531c8aabc5754ad419b2))

* feat: inherit Auth from `pydantic.AuthBase` ([`25627f2`](https://github.com/Riminder/hrflow-connectors/commit/25627f2ea1f834021621cfc231c792bf1f7bcb54))

* feat: add `PushProfileAction` class ([`ac535b6`](https://github.com/Riminder/hrflow-connectors/commit/ac535b6f26b0dfb9487940eaceeb1e976d38e4cf))

* feat: add `PushProfile` to SAP ([`79a4442`](https://github.com/Riminder/hrflow-connectors/commit/79a4442757ebb1a24b1017dc268cabddbcd8150e))

* feat: update README.md ([`7db733d`](https://github.com/Riminder/hrflow-connectors/commit/7db733dd59f2f634f762eefbe39ae996a35f212d))

* feat: add geojson in location ([`d0cdb80`](https://github.com/Riminder/hrflow-connectors/commit/d0cdb8066484acffb23620d79aaf67f7f8ea5286))

* feat: add `PullJobsAction` ([`8d95040`](https://github.com/Riminder/hrflow-connectors/commit/8d95040dd0a6bd69c7b71b90af46963453d2f1eb))

* feat: add `PushAction` ([`9d5c148`](https://github.com/Riminder/hrflow-connectors/commit/9d5c14831e17bfd03f597c7fd1e10c3bdaaa080a))

* feat: add `PullAction` ([`ea1fdcf`](https://github.com/Riminder/hrflow-connectors/commit/ea1fdcfddf6dcbc895f4323ecb2f11f54b72ff30))

* feat: add `Connector` interface ([`bdaeb62`](https://github.com/Riminder/hrflow-connectors/commit/bdaeb62c82474ff317b4ddd4f364c2b709430221))

* feat: add readme for workable connector ([`90e1766`](https://github.com/Riminder/hrflow-connectors/commit/90e176652271431f26c847fed582f060ffb008b9))

* feat: add connector for workable public endpoints jobs ([`c0a64a8`](https://github.com/Riminder/hrflow-connectors/commit/c0a64a81c80f897755dcbbaf2779a01fa85295a4))

* feat: add `PullJobsAction` ([`af9a30a`](https://github.com/Riminder/hrflow-connectors/commit/af9a30ae355fb01caa0679d27dc77501e21feed5))

* feat: add `PushAction` ([`adeeb2d`](https://github.com/Riminder/hrflow-connectors/commit/adeeb2d6ffc3b9d11a45e44695728455b013c080))

* feat: add `README.md` for SAP ([`ca4625e`](https://github.com/Riminder/hrflow-connectors/commit/ca4625e52befa896112ef076860e22cc34c60e8f))

* feat: add `PullAction` ([`a50a26c`](https://github.com/Riminder/hrflow-connectors/commit/a50a26c0cb66687f08a7d3c007fa6c004d9d9ae1))

* feat: add basemodel file for succesfactors job obj ([`e6d162c`](https://github.com/Riminder/hrflow-connectors/commit/e6d162cae1165838ade95a0a9360d3e184a6bbf3))

* feat: add `Connector` interface ([`c614f68`](https://github.com/Riminder/hrflow-connectors/commit/c614f684ec2a89a5837043fe7546be063a2c6943))

* feat: add connector for sap successfactors job requistions ([`439a819`](https://github.com/Riminder/hrflow-connectors/commit/439a819af87ee9ddb4a9647e4223912678b38958))

* feat: add successfactors SAP jobs connector ([`6d87355`](https://github.com/Riminder/hrflow-connectors/commit/6d8735500309f5a103eec1de8c42706113275a45))

* feat: add tags in format ([`dd88867`](https://github.com/Riminder/hrflow-connectors/commit/dd888674c426b944d9af198b5329fb88926dd017))

* feat: add `imports` to __init__.py ([`f4188d6`](https://github.com/Riminder/hrflow-connectors/commit/f4188d601cf0c15f36b9f8e17f377b3507364087))

* feat: add `functions` to access profile objects in `format` ([`cf68bb7`](https://github.com/Riminder/hrflow-connectors/commit/cf68bb7fd425a2876261e31a28a868cd91e97e1c))

* feat: add `PushProfile` for greenhouse ([`a619070`](https://github.com/Riminder/hrflow-connectors/commit/a6190706cf2ec871382c2205f45dee4f4b44a031))

* feat: add `PushProfile` smartrecruiters in `README.md` ([`c10a004`](https://github.com/Riminder/hrflow-connectors/commit/c10a00410ec6c3f289e7579c9b528a6d2701a2f7))

* feat: change empty values with `undefined` ([`e5f84f5`](https://github.com/Riminder/hrflow-connectors/commit/e5f84f55678345d2da153147677453f68febd51d))

* feat: add `careerbuilder` and `greenhouse` jobs connectors ([`ab620e4`](https://github.com/Riminder/hrflow-connectors/commit/ab620e4295a22128825dd1e522557840685e93cb))

* feat: add logging for `time` sleep ([`78f307c`](https://github.com/Riminder/hrflow-connectors/commit/78f307c5290f345996f0fbff3e1b551e85a724eb))

* feat: get `id` instead of `internal_id` for reference ([`ad6f271`](https://github.com/Riminder/hrflow-connectors/commit/ad6f2710887943da2ba1b9991f0d8bfa2c6cfa00))

* feat: add `HTMLParser` and `remove_html_tags` for description ([`e7b1d86`](https://github.com/Riminder/hrflow-connectors/commit/e7b1d86c0e6c940c3b0eaaf414e65153be4897b8))

* feat: add connector for `greenhouse` job boards ([`20a0c89`](https://github.com/Riminder/hrflow-connectors/commit/20a0c896410503236c4a384af8836228aa2f4608))

* feat: add options for other careerbuilder sites ([`2ca77cc`](https://github.com/Riminder/hrflow-connectors/commit/2ca77cc8420f3d74a8aa36f262d90bff32846000))

* feat: add full name for Vadhel ([`fd278a7`](https://github.com/Riminder/hrflow-connectors/commit/fd278a7c7cc6071e9a136a661626c90843b8dcad))

* feat: add `emplyment_type` methode ([`b3c51eb`](https://github.com/Riminder/hrflow-connectors/commit/b3c51eb60caeb16cb779b0baaa93f4064b907312))

* feat: add method to get `job reference` ([`6aa8c40`](https://github.com/Riminder/hrflow-connectors/commit/6aa8c40e249219d7e146c0b76a72863827aa2d8f))

* feat: change in CHANGELOG 0.2.0 ([`2399024`](https://github.com/Riminder/hrflow-connectors/commit/2399024e1d38c5b866d9e8f0dd9f1ce66e88c8d3))

* feat: change urls in Setup ([`d6bad69`](https://github.com/Riminder/hrflow-connectors/commit/d6bad695cf0ce9f766f7a00dbbcc17a7e44b97f4))

* feat: change version -&gt; 0.2.0 ([`ac4f394`](https://github.com/Riminder/hrflow-connectors/commit/ac4f394bfe80c48312abb1b3b80b9cec8610f473))

* feat: add changelog 0.1.0 &amp; 0.2.0 ([`cb2f3cd`](https://github.com/Riminder/hrflow-connectors/commit/cb2f3cdf5ab8cb4af9c36e7d74994b58d4cc52fe))

* feat: finish new README ([`720267d`](https://github.com/Riminder/hrflow-connectors/commit/720267da46a55efcf30558568e2304f60262d6be))

* feat: add names in AUTHORS ([`e8ebdf1`](https://github.com/Riminder/hrflow-connectors/commit/e8ebdf1798a1e1e1b2956d4af0b8229d09e2364b))

* feat: add `format_switcher` to choose right format ([`b3a4ec8`](https://github.com/Riminder/hrflow-connectors/commit/b3a4ec82d5b7ee319d0c801b0fb3560ac0a75781))

* feat: add logging in OAuth2 ([`d981f7f`](https://github.com/Riminder/hrflow-connectors/commit/d981f7f7ea487d36e64a1ac9c3b49350321db3e1))

* feat: add default lastname &amp; email in Crosstalent ([`649e2f6`](https://github.com/Riminder/hrflow-connectors/commit/649e2f6bd97b87d5e307a979b2720f57781b63a8))

* feat: add `sort_by_data` param for searching ([`1dcd164`](https://github.com/Riminder/hrflow-connectors/commit/1dcd1642bebccee4c091eaa7e2ca3178755758e4))

* feat: config `pytest.ini` to show coverage in term ([`218c1c7`](https://github.com/Riminder/hrflow-connectors/commit/218c1c72caf6b237faab5b6fad31c7c26830e003))

* feat: add `page scrolling` and `max_page_num` ([`fa18d74`](https://github.com/Riminder/hrflow-connectors/commit/fa18d745f847a1cc58d4fc79912bdc471b686f1c))

* feat: add logging or fix in Indeed &amp; Craigslist ([`afe111d`](https://github.com/Riminder/hrflow-connectors/commit/afe111d999c5df06985015b1c0dbc3398e614e3a))

* feat: test: add logger with basic config ([`6d67553`](https://github.com/Riminder/hrflow-connectors/commit/6d67553250d9aa2b780200073daf1e1208a6b443))

* feat: add logging in `HTTPStream` ([`a929d51`](https://github.com/Riminder/hrflow-connectors/commit/a929d516eb157a54ab7a54c390e1cdf89e73d0ee))

* feat: add logging in `src/core/action` ([`854ca50`](https://github.com/Riminder/hrflow-connectors/commit/854ca509ea6aaeb997f2ed624a429f142ee33c17))

* feat: set `archive_deleted_...=False`  for Scraper ([`e212efc`](https://github.com/Riminder/hrflow-connectors/commit/e212efc0a7009cc06b94c17eae79aa5a2d41b738))

* feat: add `__init__.py` in xml board test ([`f64437b`](https://github.com/Riminder/hrflow-connectors/commit/f64437b849463213ccf3344a318ef50a522e6160))

* feat: facilitate import &amp; def spec for connector ([`ff5dba9`](https://github.com/Riminder/hrflow-connectors/commit/ff5dba96a5cd8949f4941aaa71e4865fda7dfaf5))

* feat: add `JobsBuilder` to pull jobs from careerbuilder ([`762507c`](https://github.com/Riminder/hrflow-connectors/commit/762507ca884489f12bf400efc8a13c1d6b0c60b7))

* feat: add Flatchr connectors in README ([`2fb0a83`](https://github.com/Riminder/hrflow-connectors/commit/2fb0a836d20ea32a7f19a2e9aae18b17ff9529a2))

* feat: add SmartToken in Auth ([`4245e75`](https://github.com/Riminder/hrflow-connectors/commit/4245e75b7cd4ef9c91507379234edda91fbca2b7))

* feat: add Craigslist connector in README ([`a73b128`](https://github.com/Riminder/hrflow-connectors/commit/a73b1286dada41b5df09c4c64a9447e5c943d10a))

* feat: add logger in Craigslist Crawler ([`ab6e543`](https://github.com/Riminder/hrflow-connectors/commit/ab6e5434cb9184ab95b052ed3cc2767f68bb34d5))

* feat: add `JobsBuilder` as a connector for career builder jobs ([`16415d8`](https://github.com/Riminder/hrflow-connectors/commit/16415d80e05bb96a3860f889075e8fd07e250b8f))

* feat: add logger utils ([`2922a32`](https://github.com/Riminder/hrflow-connectors/commit/2922a32378e27b955dc3e3ffdc471d0d0759c928))

* feat: add SmartRecruiters in README ([`708e932`](https://github.com/Riminder/hrflow-connectors/commit/708e93211e55e67b57b3639f1ae2511a545b0e0a))

* feat: add basic config for logger in utils ([`f3c1932`](https://github.com/Riminder/hrflow-connectors/commit/f3c193242e151aac971a64e0579a3ff864075e18))

* feat: add logger utils ([`48faab6`](https://github.com/Riminder/hrflow-connectors/commit/48faab65ccba47c06ed40d9e84cfe1f1daf4a662))

* feat: ignore `test/connectors` and config coverage ([`8bb28fd`](https://github.com/Riminder/hrflow-connectors/commit/8bb28fd472362b9cb4f2b173a980f093a023219e))

* feat: add `format` profile function ([`326b270`](https://github.com/Riminder/hrflow-connectors/commit/326b27096b1f103b74df2537cde8d520b5273ada))

* feat: add `SmartCandidate` class to push profile ([`bde1e8b`](https://github.com/Riminder/hrflow-connectors/commit/bde1e8b4046b56d25b7b8fbff3083d21244fae35))

* feat: add connector for smartrecruiters as destination ([`495f7a9`](https://github.com/Riminder/hrflow-connectors/commit/495f7a94f3ba36f3ca0a2a7a0d6e3e8cfb4f4006))

* feat: add authentification by api key ([`264964f`](https://github.com/Riminder/hrflow-connectors/commit/264964f2021e0645883efd0dae534bacac1edc5f))

* feat: add test function for craigslist connector ([`f70c6b0`](https://github.com/Riminder/hrflow-connectors/commit/f70c6b0d33db4ae816aca73194c97cb63f2ac08b))

* feat: add `pull` function ([`67f3228`](https://github.com/Riminder/hrflow-connectors/commit/67f3228c7318a485899be2efb8ad81b2e1c00e85))

* feat: add `Crawler` adn `base_url` funcs to `Craigslist` class ([`9f59cff`](https://github.com/Riminder/hrflow-connectors/commit/9f59cff6a0114476b4d07a3b56620e748e330bb4))

* feat: new connector for craigslit job postings ([`5f90b53`](https://github.com/Riminder/hrflow-connectors/commit/5f90b53f2ce3f7a846f06482b304114fc9e50eba))

* feat: add a `SmartToken` class for smartrecruiters connectors auth ([`cffc8ee`](https://github.com/Riminder/hrflow-connectors/commit/cffc8eea95c8be18db62c18d6da0b791a220605c))

* feat: &#39;test_SmartJobs&#39; in smart recruiters test ([`dbb776f`](https://github.com/Riminder/hrflow-connectors/commit/dbb776f565e79914d9dda0276303809199de179a))

* feat: new connector for SmartRecruiters jobs ([`9ac454d`](https://github.com/Riminder/hrflow-connectors/commit/9ac454debc4eaa87bd7e79a3f6405e55ee3da238))

* feat: add XMLBoardAction with Samsic as test ([`e126063`](https://github.com/Riminder/hrflow-connectors/commit/e126063bba6f474b2b393bc89e8cc404cbe689ea))

* feat: add pandas for tests in date/time converter ([`2a31c5b`](https://github.com/Riminder/hrflow-connectors/commit/2a31c5b7804bf305308a7bac557069e5eb05092e))

* feat: add datetime converter ([`15519ca`](https://github.com/Riminder/hrflow-connectors/commit/15519ca6ef613d6b3c3abf29f7f4625d56bf58a4))

* feat: add timedelta converter ([`b2c1e99`](https://github.com/Riminder/hrflow-connectors/commit/b2c1e99593fcdc4c7cc009f7669b8aac81fa6704))

* feat: edit job after unarchiving ([`f2333f9`](https://github.com/Riminder/hrflow-connectors/commit/f2333f9899346ea88fc5c9ded1a92583ae253896))

* feat: `selenium` in Setup.cfg ([`d99dc0a`](https://github.com/Riminder/hrflow-connectors/commit/d99dc0aaa4fc43475d0a0ec56729c6fcf10739e6))

* feat: rename `GetAllJobs` to `IndeedFeed` in tests ([`f72912f`](https://github.com/Riminder/hrflow-connectors/commit/f72912fdebcbdf862e93f5ca0051c449778d3aa3))

* feat: add `binary_location` and `max_page` as parameters in IndeedFeed ([`c26bcd7`](https://github.com/Riminder/hrflow-connectors/commit/c26bcd7d1fe626e1801ccb94bf40370918f2de46))

* feat: add `source_to_listen` in EventParser ([`aca691b`](https://github.com/Riminder/hrflow-connectors/commit/aca691beca49dad01e988879efd5f0ec818c883b))

* feat: add EvenParser with `get_profile` ([`59f2b82`](https://github.com/Riminder/hrflow-connectors/commit/59f2b827b89c5e176d0c1eda8ef5ba071276e190))

* feat: Profile obj in PushProfile &amp; ProfileDestinat ([`a0cd53d`](https://github.com/Riminder/hrflow-connectors/commit/a0cd53dd3118ff96ef1377a4332e052297df0d72))

* feat: add executable_path of the driver as parameter to the connector ([`9febc84`](https://github.com/Riminder/hrflow-connectors/commit/9febc84fc6798dfd46c10fb6df10f82953c2524b))

* feat: add type accepted by `format` and `pull` ([`6770205`](https://github.com/Riminder/hrflow-connectors/commit/6770205614eda03b5ca655215f364b7fe13fc0bb))

* feat: no-debug: add Crosstalent PushProfile ([`4992b40`](https://github.com/Riminder/hrflow-connectors/commit/4992b406af8678df093b2d325e6725680c91ae7e))

* feat: adding selenium dependency ([`f0f1a44`](https://github.com/Riminder/hrflow-connectors/commit/f0f1a44635538c39ecfe9c738ea42c51cb16987d))

* feat: get all jobs with indeed scrapper ([`6156bf0`](https://github.com/Riminder/hrflow-connectors/commit/6156bf0e8e51af23df14db0dd1403219e01f928a))

* feat: option `archive_deleted_jobs_from_stream` ([`14ae5ed`](https://github.com/Riminder/hrflow-connectors/commit/14ae5ed81fe1222385cd6fd46e803a788a4131cf))

* feat: `check_deletion_...` and update `execute` ([`c067c27`](https://github.com/Riminder/hrflow-connectors/commit/c067c27c7f5d5ea08fb58e1813e8c175b2da3961))

* feat: nodebug: check_reference_in_board before add ([`c0f2fe6`](https://github.com/Riminder/hrflow-connectors/commit/c0f2fe6041aec10a24e6703740b274e44a5ccf27))

* feat: get all references from Board funct ([`62eb711`](https://github.com/Riminder/hrflow-connectors/commit/62eb711fa223854bf94386c1b390ec6a3dafa373))

* feat: `hydrate_job_with_parsing` ([`ce311ed`](https://github.com/Riminder/hrflow-connectors/commit/ce311ed7bdece8119719ed42acce432734b970fe))

* feat: add remove_hrml_tag ([`06e9529`](https://github.com/Riminder/hrflow-connectors/commit/06e9529eb7b3685cc8803d0b134e55f57f867d06))

* feat: generic format &amp; add hr job wrapper ([`c4bae62`](https://github.com/Riminder/hrflow-connectors/commit/c4bae62a80c18f2078702d05aeca5cf036b5c51a))

* feat: add interface for `hydrate_with_parsing` ([`e6bca99`](https://github.com/Riminder/hrflow-connectors/commit/e6bca998bfb7df05fd47e29ed88878799bef47c3))

* feat: subdomain option in GetAllJobs Crosstalent ([`45e9d22`](https://github.com/Riminder/hrflow-connectors/commit/45e9d22419ce4ce5f16100c1dc52bb5c360b9e20))

* feat: add VERSION in package ([`4907ac4`](https://github.com/Riminder/hrflow-connectors/commit/4907ac4a6b3d80d9efe9eeba9a36ede0ccca7556))

* feat: change CONTRIBUTING StructuredText -&gt; Markdown ([`86d1606`](https://github.com/Riminder/hrflow-connectors/commit/86d1606e152058875c0a1a43ef5cbebd7cb11162))

* feat: change CHANGELOG StructuredText -&gt; Markdown ([`eba10ac`](https://github.com/Riminder/hrflow-connectors/commit/eba10ac2f3058d3bbf0483fa22c21a0b5dbcdcd3))

* feat: change AUTHORS StructuredText -&gt; Markdown ([`f16152b`](https://github.com/Riminder/hrflow-connectors/commit/f16152be8ea625de4c16f61694c9716d494c3e99))

* feat: add Spec to Crosstalent ([`daf4d7f`](https://github.com/Riminder/hrflow-connectors/commit/daf4d7f907f8a2e350bba644f9d2572d87e73df7))

* feat: add dependancies in `setup.cfg` ([`9910613`](https://github.com/Riminder/hrflow-connectors/commit/99106134f571edd15b2acf2e36f0825b558522d9))

* feat: add naive format and pull for GetAllJob ([`b4cb403`](https://github.com/Riminder/hrflow-connectors/commit/b4cb4033e9cfb9b9c66d839e1baec4eaab0da45e))

* feat: inherit GetAllJobs Action from BoardAction ([`e060e36`](https://github.com/Riminder/hrflow-connectors/commit/e060e365572f8d520bdae5b27cce7765a5fb2caa))

* feat: no-debug: create BoardAction ([`505aeb7`](https://github.com/Riminder/hrflow-connectors/commit/505aeb77b0d6411f9c028d223e49cca3f274e74f))

* feat: use iterator and optimize run ([`8d38ac7`](https://github.com/Riminder/hrflow-connectors/commit/8d38ac741e393f72b1a18c0466c867724a1865af))

* feat: add interface `connect` and write `execute` ([`d2405f9`](https://github.com/Riminder/hrflow-connectors/commit/d2405f9c1d96d90e8f59e0634d72e7c0ef2f0fa1))

* feat: add apply_filters function in Action ([`924c4f0`](https://github.com/Riminder/hrflow-connectors/commit/924c4f04cc988eef535f0b345f2360f0662f8553))

* feat: add test notebook in .gitignore ([`e153d56`](https://github.com/Riminder/hrflow-connectors/commit/e153d562ac33b04ac81bb2fcae6746828b03214e))

* feat: create Action abstract class ([`67d003d`](https://github.com/Riminder/hrflow-connectors/commit/67d003dd80354b5c103bb474aa4ab165b12b2a08))

* feat: use fixture in crosstalent tests ([`73ef896`](https://github.com/Riminder/hrflow-connectors/commit/73ef896264cac48ef68cbf077e28d79ee9c462c0))

* feat: mock HTTPAction test ([`a441972`](https://github.com/Riminder/hrflow-connectors/commit/a441972dd93b607cee8a23d6ee520afea74e7f82))

* feat: mock request in test_auth ([`7fd036a`](https://github.com/Riminder/hrflow-connectors/commit/7fd036a2d016d51126c962ecc15637d2278a7b1a))

* feat:  add crosstalent HTTP call ([`d923e2a`](https://github.com/Riminder/hrflow-connectors/commit/d923e2aa66c4d2f4c3e846af9c70841e739dea35))

* feat: update HTTPAction &amp; Auth to BaseModel ([`53ef314`](https://github.com/Riminder/hrflow-connectors/commit/53ef3143c5cae8953ce5402e1675b85ef51d63f2))

* feat: add HTTPAction ([`c8f0c81`](https://github.com/Riminder/hrflow-connectors/commit/c8f0c8130d35f3c057b07a3ebe5bc44ce36f53d3))

* feat: add OAuth2 ([`1ee385c`](https://github.com/Riminder/hrflow-connectors/commit/1ee385ccb277e529fd21a1a9f9818cf4b56de510))

* feat: add pydantic and dev package with poetry ([`0feaaa5`](https://github.com/Riminder/hrflow-connectors/commit/0feaaa54a5b871060b63a1a4c8a53373b9cd313e))

### Fix

* fix: Update Action template to add align=center wrapping and use singular Action column title ([`3279647`](https://github.com/Riminder/hrflow-connectors/commit/3279647997909ad50b55e3468bfc6b34d7160bb6))

* fix: Correct link to documentation for action ([`69f1837`](https://github.com/Riminder/hrflow-connectors/commit/69f18378e6b411c2dc46e4bc8974116c5d62dd78))

* fix: Remove what seems to be duplicate same action in Waalaxy and update related code ([`c0c2297`](https://github.com/Riminder/hrflow-connectors/commit/c0c2297c03c2fcf00402f64c8e1390b1fcd07920))

* fix: Update core tests for ActionName and ActionType ([`8ee7bf7`](https://github.com/Riminder/hrflow-connectors/commit/8ee7bf7ba4585433262455b63af1d568fdfde595))

* fix: Update manifest generation to work with ActionName enum ([`6979211`](https://github.com/Riminder/hrflow-connectors/commit/697921127175203fd385f0e517649a441ffe442c))

* fix: Force requirements content to be written to disk to fix bug in nox session running without deps ([`59b9f1e`](https://github.com/Riminder/hrflow-connectors/commit/59b9f1e3c00c6ba9646d53888d4bd73d8e413897))

* fix: Abort action when format or logics fails for all items ([`87da16a`](https://github.com/Riminder/hrflow-connectors/commit/87da16ac534a16ce0d6fb08a6edcd126a0ae99cf))

* fix: Correct doc generation after using action name as Enum ([`be8e6ea`](https://github.com/Riminder/hrflow-connectors/commit/be8e6ea67b14d482c0ddb00ca8c5bb68404f1fe8))

* fix: Fix poetry run command that needs to pass argument to nox session ([`b5a69b0`](https://github.com/Riminder/hrflow-connectors/commit/b5a69b0201c0558fd9cced7c772d20402b0a6da1))

* fix: Add  when example is string for proper templating ([`a8c269a`](https://github.com/Riminder/hrflow-connectors/commit/a8c269aa9ef516d9018297a4c0f32be2608724d5))

* fix: Update forgotten Python version in DOCUMENTATION.md ([`d6c839b`](https://github.com/Riminder/hrflow-connectors/commit/d6c839bab77dfec4abb82a024bacf129444f6eb9))

* fix: Remove event_parser from being displayed in docs as it is only used in the No Code setup ([`5ecf46a`](https://github.com/Riminder/hrflow-connectors/commit/5ecf46afc8a76fdf080a26de9bd4b14fd6d4d9af))

* fix: Correct template code to properly use event_parser when there is a default implementation and update manifest accordingly ([`476ddcb`](https://github.com/Riminder/hrflow-connectors/commit/476ddcb4acdb51186c70e8358af71b451a6ea2b1))

* fix: Remove code needed for local test that was commited by mistake ([`840cc5c`](https://github.com/Riminder/hrflow-connectors/commit/840cc5c43398dbaa2009d9a68a4af7d2372b37d6))

* fix: Filter not mandatory for TalenstSoft read profiles ([`c151bb8`](https://github.com/Riminder/hrflow-connectors/commit/c151bb8380e36978d205e39290b649e1c34bb040))

* fix: Bug in proper testing of event counts ([`818eea6`](https://github.com/Riminder/hrflow-connectors/commit/818eea61ecfbb9420faab31ed84df14c74a8c0ec))

* fix: Correct Iterator -&gt; Iterable ([`4cca3ad`](https://github.com/Riminder/hrflow-connectors/commit/4cca3adfa8c451bf84dcad6e75b8d5aec467f7d0))

* fix: Remove secrets from core tests steps because not needed ([`3bcb7e5`](https://github.com/Riminder/hrflow-connectors/commit/3bcb7e59a08ea49ee792ca4970aeed97bf6db846))

* fix: Correct path of global secrets file in Actions ([`dbd91bc`](https://github.com/Riminder/hrflow-connectors/commit/dbd91bcc41be6ba3e4916d072414a6457714e7ed))

* fix: Use python version 3.8.0 ([`0b9354b`](https://github.com/Riminder/hrflow-connectors/commit/0b9354b96188fe2b41f683f0e75438c6a70ee841))

* fix: Correct path to global secrets file ([`3de1eb9`](https://github.com/Riminder/hrflow-connectors/commit/3de1eb94b3c335f68b73cb876d2007bdabd9768f))

* fix(bug): replace `Profile` and `Job` with `HrflowProfile`, `HrflowJob` ([`42d37ad`](https://github.com/Riminder/hrflow-connectors/commit/42d37ad740b93b31db2fc7da9146c5980d598e0c))

* fix: remove `Job` and `Profile` from utils/Hrflow ([`707dc5c`](https://github.com/Riminder/hrflow-connectors/commit/707dc5c18af6c2f3be976a29969a3f72e976f599))

* fix: remove selenium from setup.cfg ([`947cb2a`](https://github.com/Riminder/hrflow-connectors/commit/947cb2a23dc4f6f65e463b4403f7a7c50dbb2b7d))

* fix: flatch push_profile doc and bullhorn TalentDataType actions ([`b5af289`](https://github.com/Riminder/hrflow-connectors/commit/b5af28953bb46546e5b755381d3bd9bf855cfc41))

* fix: remove .dict() from function push in looping over enrich lists ([`83152fc`](https://github.com/Riminder/hrflow-connectors/commit/83152fc7111c47e9a840221ec824b89116c3c261))

* fix: schemas in bullhorn ([`a1f8877`](https://github.com/Riminder/hrflow-connectors/commit/a1f88773c0153dd6081fd415cd50cbdcefa3e934))

* fix: schemas in Bullhorn ([`c81cdcc`](https://github.com/Riminder/hrflow-connectors/commit/c81cdcc2e57326435f7c41c188585e1ceb3e5827))

* fix: typo in get_section func ([`649113e`](https://github.com/Riminder/hrflow-connectors/commit/649113e010fc1e50b06ac8ed13e016d88807d76d))

* fix: update attachments and consent in smart r schemas ([`9c1ce92`](https://github.com/Riminder/hrflow-connectors/commit/9c1ce9234fccdce5d5a9d925db9e5e8e2b291af3))

* fix: add schemas in teamtailor actions ([`0826c25`](https://github.com/Riminder/hrflow-connectors/commit/0826c256ffc33f1d9336c45de4f5197dfad6e90c))

* fix: schemas in smartr ([`7639fd0`](https://github.com/Riminder/hrflow-connectors/commit/7639fd0376399758f545c7b74a633b455b2adca9))

* fix: remove requied parms from workable schema ([`41fe092`](https://github.com/Riminder/hrflow-connectors/commit/41fe0925731f40cac57e9e02124b471c36818991))

* fix:  schemas in Flatchr ([`fa9e406`](https://github.com/Riminder/hrflow-connectors/commit/fa9e4065796a6cd5f85141f25ca1ac6f3f0d4d8e))

* fix: add profile basemodel in connector tests ([`d52288f`](https://github.com/Riminder/hrflow-connectors/commit/d52288fff591f8ce6cb1a4cea5dc8438ba54b2a1))

* fix: remove deprecated properties from sap candidate schemas ([`596babc`](https://github.com/Riminder/hrflow-connectors/commit/596babc4e6ff9ea1a1f13f99af3a5b5d0ee7a967))

* fix: profile validation error in push profile base action ([`11dd960`](https://github.com/Riminder/hrflow-connectors/commit/11dd960741817ec98486e3caa12381ef0b51f836))

* fix: Attribute objects to None in hrflow schemas ([`9f708bc`](https://github.com/Riminder/hrflow-connectors/commit/9f708bc27af4218ab4a91a33038ff6f15bb3af33))

* fix: put all attributes too optional to avoid validation error when pushing a profile ([`fb27c67`](https://github.com/Riminder/hrflow-connectors/commit/fb27c6723d8e7250f36fb834c16f7e0e290cfd66))

* fix: schemas requirements in contract length and work hours ([`4000d12`](https://github.com/Riminder/hrflow-connectors/commit/4000d125d5ebaa5d0293775d0eb8ead0f3fa2083))

* fix: metadatas validation error in crosstalent format ([`2781a3b`](https://github.com/Riminder/hrflow-connectors/commit/2781a3bd39a520431be619e1f575010044e69364))

* fix: job[&#34;id] -&gt; friendly_id in format ([`9130273`](https://github.com/Riminder/hrflow-connectors/commit/91302736130b3e67aaa2175c61f4560376204bde))

* fix: replace job.get(reference) with job.reference in stream ([`7b29142`](https://github.com/Riminder/hrflow-connectors/commit/7b291420f5a6a316d3f1dbd5680e440a3ae154a7))

* fix: remove None from required fields in Job model ([`978ce9b`](https://github.com/Riminder/hrflow-connectors/commit/978ce9bb2081450404eba355aed4c3d142513c8a))

* fix: remove ``None` default values from hrflow objects required fields ([`7709090`](https://github.com/Riminder/hrflow-connectors/commit/77090907b95e514f8206f99850e39694f0ed6844))

* fix: rename Push...Action-&gt;PushProfileBaseAction ([`7e500f4`](https://github.com/Riminder/hrflow-connectors/commit/7e500f469b6f21dc6c9a2c73620ccc8a5b3047dc))

* fix: fix typing requirements in smartrecruiters objects schemas ([`58b3fff`](https://github.com/Riminder/hrflow-connectors/commit/58b3fff536a4b2b12d33a6c040a9ebc5adb6219f))

* fix: remove print(job) in get_full_job ([`3ebdeaf`](https://github.com/Riminder/hrflow-connectors/commit/3ebdeafa2b83884c3496a321ba66f22eeac4681a))

* fix:  typing mistakes and typos in aliases in schemas ([`5a7c5a6`](https://github.com/Riminder/hrflow-connectors/commit/5a7c5a609d60b7c923fa15c713225ba39dd13f0f))

* fix: missing optional types in sap profile schema ([`6e9522e`](https://github.com/Riminder/hrflow-connectors/commit/6e9522eee37ec16cb387f79d844afc07871e8273))

* fix: fix typing in greenhouse profile coordinator schema ([`b12fd57`](https://github.com/Riminder/hrflow-connectors/commit/b12fd5751f80194aadd71bfe3a4eb004b4997cdd))

* fix: remove print in pull function for ceridian ([`e7558fa`](https://github.com/Riminder/hrflow-connectors/commit/e7558fa1ec7dae133f0d3147359fdc5306ae3a32))

* fix: fix schemas for ceridian ([`9f1477b`](https://github.com/Riminder/hrflow-connectors/commit/9f1477b28332131f75f05eec289d01c84a73644d))

* fix: fix dates typing in schemas and format ([`685eb3b`](https://github.com/Riminder/hrflow-connectors/commit/685eb3b55fc39a57349d1f5181d61d1f87d6cfd8))

* fix: remove unnecessary typing imports: list, dict, any ([`3e92f3c`](https://github.com/Riminder/hrflow-connectors/commit/3e92f3c1e492af4cebc7fea1a166e0482662a944))

* fix: types in crosstalent job model ([`9ed7173`](https://github.com/Riminder/hrflow-connectors/commit/9ed71739339eb3a7c13c40b0585d76836c063ba0))

* fix: remove lib token ([`dc9205e`](https://github.com/Riminder/hrflow-connectors/commit/dc9205e6482a8f180b139768b9c83f89cf7df44b))

* fix: remove duplicate imports ([`b3194fd`](https://github.com/Riminder/hrflow-connectors/commit/b3194fd3abb57d2d246b1f9e45f727ea333e5319))

* fix: put default values as None in Optional model objects ([`feb35c2`](https://github.com/Riminder/hrflow-connectors/commit/feb35c29fb08e5365de42973d047f3539664fe55))

* fix: correct typos and remove unnecessary parameteres in `send_request` ([`cfcde16`](https://github.com/Riminder/hrflow-connectors/commit/cfcde16d9d416543aa95a82e1bc06ab1284909c2))

* fix: break company[&#34;experience&#34;] line in formate/ remove return_response param in push ([`e4ca5ca`](https://github.com/Riminder/hrflow-connectors/commit/e4ca5cadcb15fc065d1df288260c97b80c17a76e))

* fix: add `PATCH` request method to update profile ([`183f856`](https://github.com/Riminder/hrflow-connectors/commit/183f856ae7dd1b3a454b42b40609075cae38eb17))

* fix: remove `logger.debug` in Breezy ([`dec8aa0`](https://github.com/Riminder/hrflow-connectors/commit/dec8aa0cbb98a379ff1ebe15b62bda9c34112113))

* fix: delete duplicate contributing file ([`d72ccee`](https://github.com/Riminder/hrflow-connectors/commit/d72cceee29e4c7c6c840e865a79bea8f0835e49f))

* fix: remove unnecessary import `html` ([`0989c13`](https://github.com/Riminder/hrflow-connectors/commit/0989c13c607e1fa690dc5dcccb1c2225489b7c50))

* fix: bug in profile education ([`eb4801d`](https://github.com/Riminder/hrflow-connectors/commit/eb4801d178073e4bcb50be8e04aa805ea9959dab))

* fix: pushes -&gt; push ([`79bf234`](https://github.com/Riminder/hrflow-connectors/commit/79bf2344cd77bab8cec3a60a4fea60ddf291e28e))

* fix: fix bug in sap push profile ([`d26592a`](https://github.com/Riminder/hrflow-connectors/commit/d26592ab0b7caafe93fb1113a6eba9aa527903b4))

* fix: README in using heavy check in array ([`4bd7fb1`](https://github.com/Riminder/hrflow-connectors/commit/4bd7fb13bd07e65425e7ed5eac233ac2aefeeff4))

* fix: remove import `webdriver-manager` in conftest ([`db35cf6`](https://github.com/Riminder/hrflow-connectors/commit/db35cf666e6e96a18b00e38d4ba215e50d85b766))

* fix: forget to rename `XMLPullJobsAction` in tests ([`5badf6d`](https://github.com/Riminder/hrflow-connectors/commit/5badf6df6335ae43cadaa3b7b622b087cb9242ad))

* fix: test: add `hrflow_client` for `Action` ([`fc3537c`](https://github.com/Riminder/hrflow-connectors/commit/fc3537c384415d5d45362c461e8d58e1663bda2b))

* fix: test: add `hrflow_client` for `Action` ([`1ff5fe9`](https://github.com/Riminder/hrflow-connectors/commit/1ff5fe93a33e2f5f24068a7886dc1c4ca97aa509))

* fix: `Index Error` corrected in  `format` ([`3f2a8dd`](https://github.com/Riminder/hrflow-connectors/commit/3f2a8dd6f6e07905eaeff15e78a4f8fd50426a39))

* fix: doc: link for Crosstalent `GetAllJobs` ([`4f747cf`](https://github.com/Riminder/hrflow-connectors/commit/4f747cfe4fec770df8102a25de7d1e86a77c5118))

* fix: doc: spelling in `hydrate_with_parsing` ([`5ab92bc`](https://github.com/Riminder/hrflow-connectors/commit/5ab92bc698a29fd134ba8ec777c5bcd6c81cae72))

* fix: doc: typo `pull` -&gt; `push` ([`7b8ea87`](https://github.com/Riminder/hrflow-connectors/commit/7b8ea87e3c65fb72c01c95acb48223e06ac1fe11))

* fix: doc: remove `actions` in import example ([`ff0295c`](https://github.com/Riminder/hrflow-connectors/commit/ff0295c371a331396c3214374b6d9dfba74164e6))

* fix: return in example in README for `Crosstalent` ([`9218bb0`](https://github.com/Riminder/hrflow-connectors/commit/9218bb0e01e4806860821357ccc5e5c540a4fdd2))

* fix: move `execute`  to `ProfileDestinationAction` ([`94eda85`](https://github.com/Riminder/hrflow-connectors/commit/94eda8580a369e685bc424585f87658ddf12d21c))

* fix: parse after filtering jobs already in Board ([`2ca4d38`](https://github.com/Riminder/hrflow-connectors/commit/2ca4d389aea1e51859e4af582c769588df4a3879))

* fix: test: activate parsing in crosstalent tests ([`c78eb77`](https://github.com/Riminder/hrflow-connectors/commit/c78eb776f573832db0158c53418581e924518fe2))

* fix: update hrflow_client fixture in indeed tests ([`d53c47b`](https://github.com/Riminder/hrflow-connectors/commit/d53c47b9b3fd417ffafd74cb5efecd1c79bb4e29))

* fix: add `ElementClickInterceptedException` in `pull` clicks ([`0e3e579`](https://github.com/Riminder/hrflow-connectors/commit/0e3e579a28d769edccd73b0f56c48c7e523cb171))

* fix: remove ROOT_PATH &amp; use pytestconfig ([`8089688`](https://github.com/Riminder/hrflow-connectors/commit/808968851f18844e5e6da85b0244c541154c941c))

* fix: edit `format` function ([`a606a8b`](https://github.com/Riminder/hrflow-connectors/commit/a606a8b612a59931cb98d3e3fa7d4c55d9ed6e7d))

* fix: remove useless `Session` ([`98ccba1`](https://github.com/Riminder/hrflow-connectors/commit/98ccba1549c0c81f6587b1cc331efab2c9d4654d))

* fix: replace `ROOT_PATH` by `pytestconfig` ([`dfed756`](https://github.com/Riminder/hrflow-connectors/commit/dfed75698e47519bece58ba2ab29728a002a4fb6))

* fix: fix type validation erros in in date_start and date_end, test passed ([`f4f088d`](https://github.com/Riminder/hrflow-connectors/commit/f4f088d2d9bbbd5ac72455bb8c10995ab9f2bdd8))

* fix: rename `JobsBuilder` to `CareerJobs` in tests ([`43829df`](https://github.com/Riminder/hrflow-connectors/commit/43829df27581877510d8934b900fa7838abb096d))

* fix: edit the `Crawler` function ([`788fb19`](https://github.com/Riminder/hrflow-connectors/commit/788fb19464e1f887ed38d984cff9bad762eb55bf))

* fix: edit the `Crawler function` ([`7aeb047`](https://github.com/Riminder/hrflow-connectors/commit/7aeb047d914fb20f54f8bfae9ed791430527f8b2))

* fix: delete career builder from master ([`89357b7`](https://github.com/Riminder/hrflow-connectors/commit/89357b7cbe1eea2b4196aa02425af0dce003e830))

* fix: format in smart recru connector ([`37c43ea`](https://github.com/Riminder/hrflow-connectors/commit/37c43eae8a891b8a6f94dc653ef7569e2f4a0ac6))

* fix: test: forget to remove `assert_datetime` ([`7938716`](https://github.com/Riminder/hrflow-connectors/commit/793871636c0dd8e51d654a016d46523de845d404))

* fix: move `python_requires` to `[options]` ([`3d4e308`](https://github.com/Riminder/hrflow-connectors/commit/3d4e308adee220ee4bd21b943ab7bc798ba00d08))

* fix: remove useless pull overwrite enrichment ([`d1d421d`](https://github.com/Riminder/hrflow-connectors/commit/d1d421d37924fe7a0ee28c93b94f68c1de024dde))

* fix: remove useless pull overwrite ([`a7747b8`](https://github.com/Riminder/hrflow-connectors/commit/a7747b8728a9f15ac84d4fc989f1e30c469a7268))

* fix: tags fields in `format`, correct the values ([`cbf60ed`](https://github.com/Riminder/hrflow-connectors/commit/cbf60ed099c60dac7cb60c500799bfdbec8f432d))

* fix: rename job tags and sections in `format` ([`7985ebb`](https://github.com/Riminder/hrflow-connectors/commit/7985ebba10b3d2175444d5a0e167f7289df80298))

* fix: import re.match instead of importing all the library ([`159c2ea`](https://github.com/Riminder/hrflow-connectors/commit/159c2ea1e25a37311865994e34494839cd516cf9))

* fix: correct typos in job `format` keys ([`fa569ae`](https://github.com/Riminder/hrflow-connectors/commit/fa569ae4e324ae915f03b86ea74c6c591b707ff7))

* fix: `base_url` in response_job ([`313fadf`](https://github.com/Riminder/hrflow-connectors/commit/313fadf3bd7fb4c750c3967c8ff345626b797e3a))

* fix: delete xstr lambda function ([`dd9e2a7`](https://github.com/Riminder/hrflow-connectors/commit/dd9e2a7e9b64620ef05c047868b644c71a82f4c4))

* fix: add content type to `headers` in `pull` function ([`5ed5081`](https://github.com/Riminder/hrflow-connectors/commit/5ed5081a48e8ba600e198453d4b4df97d504cddc))

* fix: add descriptions to `limit`, `offset` in `SmartJobs` ([`9eab21c`](https://github.com/Riminder/hrflow-connectors/commit/9eab21cf908e3f9c7cecc6d831cd518e7afe3b28))

* fix: remove datetime and HTTPstream unnecessary dependencies ([`c041b62`](https://github.com/Riminder/hrflow-connectors/commit/c041b6205118ee2f4fd4db583bca4f76bb6b5f34))

* fix: add `__init__.py` in utils folder ([`2fcd0ba`](https://github.com/Riminder/hrflow-connectors/commit/2fcd0baefd5bdcf63dd930fd2f2e9811e2ed9a8e))

* fix: correct jobreference search in &#39;format&#39; ([`5be95a9`](https://github.com/Riminder/hrflow-connectors/commit/5be95a916f3d51316f97ae6b2442f117e673f370))

* fix: import xml.etrree &amp; update test push profile ([`d798824`](https://github.com/Riminder/hrflow-connectors/commit/d7988246ea0ea746e2ad258d51e13e229440b3e0))

* fix: connexion to driver wasn&#39;t opened due to a driver.quit() in pull func ([`8aca7a4`](https://github.com/Riminder/hrflow-connectors/commit/8aca7a454f0045d2f4bbb101510ae8cce7f19221))

* fix: remove private attr in HTTPStream ([`184b843`](https://github.com/Riminder/hrflow-connectors/commit/184b84355289deb26b16b3f68b26c8fd963f84f1))

* fix: doublon field when hydrating job with parsing ([`e9149e7`](https://github.com/Riminder/hrflow-connectors/commit/e9149e77aa97de9ea32c23bda04e493f380b063f))

* fix: location field, use geojson ([`32500d7`](https://github.com/Riminder/hrflow-connectors/commit/32500d73b0989dec59a36e17cc81017b61d9e918))

* fix: remove abstraction and explicit in format ([`5368136`](https://github.com/Riminder/hrflow-connectors/commit/536813619615829acd9a6e25d2257cebe1311bf4))

* fix: importlib and add python version required ([`d1bc152`](https://github.com/Riminder/hrflow-connectors/commit/d1bc1520cb53c0e3a46acf46fd8e6d66bfcfa8e8))

* fix: long_description_content_type in Setup ([`e8cffdb`](https://github.com/Riminder/hrflow-connectors/commit/e8cffdb5ed824828e9eeb534cddadf1ef1485b68))

* fix: README ([`9108e1d`](https://github.com/Riminder/hrflow-connectors/commit/9108e1dd4ffdabc8727d1acdf564f4f0e3bc64e2))

* fix: test rename `connect` -&gt; `format` ([`f4ea648`](https://github.com/Riminder/hrflow-connectors/commit/f4ea648234f9f7b38d5c5f4beb0fe22c413af32a))

* fix: add credentials  and checkpoints in gitignore ([`24eb32e`](https://github.com/Riminder/hrflow-connectors/commit/24eb32ed64335608217abac42a1c74c86e747d4f))

* fix: name, email, urls, path in setup.cfg ([`b49990e`](https://github.com/Riminder/hrflow-connectors/commit/b49990e7d48f399260efd8c5813f6b3e215689cf))

* fix: re-generate pyproject and add some packages ([`5b9f6b1`](https://github.com/Riminder/hrflow-connectors/commit/5b9f6b177f3ad2a9f642e73dd0301220e308c842))

### Performance

* perf: optimize auth use ([`d5a9e5d`](https://github.com/Riminder/hrflow-connectors/commit/d5a9e5dc34784e8f616859328e77cff522117b45))

* perf: opti RAM usage by iterating on job pages ([`e5fcc87`](https://github.com/Riminder/hrflow-connectors/commit/e5fcc87023ba7dc8ec890a67ecb2f3e5af5c2be5))

* perf: get job links in a comprehension list in `pull` ([`8d89cc9`](https://github.com/Riminder/hrflow-connectors/commit/8d89cc9f8ab92c089a2dac04691cafe2052f70e5))

### Refactor

* refactor: Use all purpose read profile TS warehouse rather than overfitted function for Safran catch workflows with default event_parser ([`08cf0b5`](https://github.com/Riminder/hrflow-connectors/commit/08cf0b565d0ecb43bf2c2d52eb4a5e52d80ec93d))

* refactor: Split HrFlow warehouses into separate job and profile modules ([`7d556ed`](https://github.com/Riminder/hrflow-connectors/commit/7d556ed454ec5a8d8cf7729796c579aa9f86e1d6))

* refactor: Rename pull_parameters_from -&gt; parameters ([`c380d9a`](https://github.com/Riminder/hrflow-connectors/commit/c380d9a03edfec528651dee6cfaba91094ef473a))

* refactor: Rename ActionRunResult -&gt; RunResult ([`f52c3aa`](https://github.com/Riminder/hrflow-connectors/commit/f52c3aa5a87f1f1d99c755220ad77342331d4691))

* refactor: modify GetAllJobs into HrFlow connector format ([`fb549a3`](https://github.com/Riminder/hrflow-connectors/commit/fb549a3e7a12e28fb3747255ad9800b02cd9bed2))

### Style

* style: Minor formatting changes ([`d363f39`](https://github.com/Riminder/hrflow-connectors/commit/d363f3947af15b0e3bab764a854771506ac9b5a7))

* style: Apply black and sort imports ([`0495bdb`](https://github.com/Riminder/hrflow-connectors/commit/0495bdbefe917a9e7d4b5a1b961047abced37792))

* style: Run black ([`ec81c27`](https://github.com/Riminder/hrflow-connectors/commit/ec81c2723eb4d34d5969abccb34955da9d2b9116))

* style: reformat code modules to PEP8 ([`a8ae35f`](https://github.com/Riminder/hrflow-connectors/commit/a8ae35fc53f128e82bffb51ec764c6c5ca3d2f3f))

* style: add docstrings for push_profile ([`cbb7d05`](https://github.com/Riminder/hrflow-connectors/commit/cbb7d05edc5e4cb3a755faf27b4954b3512651c9))

* style: reformat to PEP8 ([`bed9794`](https://github.com/Riminder/hrflow-connectors/commit/bed9794d2f1fc217b078687b8a593ac06ccc99d5))

* style: reformat to PEP8 ([`253f901`](https://github.com/Riminder/hrflow-connectors/commit/253f9015b266e240e7cac22cf9feae3d41fdb60f))

* style: remove useless headers content-type ([`b55a3ff`](https://github.com/Riminder/hrflow-connectors/commit/b55a3ffac47047531abbbf452245765f5bd9b88d))

* style: remove useless print ([`a2ed4d1`](https://github.com/Riminder/hrflow-connectors/commit/a2ed4d1abc476feb714e3cce4bd5f3537c7bd020))

* style: delete useless certifications ([`9887ffe`](https://github.com/Riminder/hrflow-connectors/commit/9887ffe7fdd7e143072806de64530a379b90ad39))

* style: reformat with black ([`e4adb1b`](https://github.com/Riminder/hrflow-connectors/commit/e4adb1bfe77ab68ac95e6d2b74e68b9eb30bbb3a))

* style: format to PEP8 ([`865b5a0`](https://github.com/Riminder/hrflow-connectors/commit/865b5a0071e966aa18d9982c178fdb6fd0ebd229))

* style: remove useless prints ([`e213800`](https://github.com/Riminder/hrflow-connectors/commit/e213800e87c7c9d99807fca2ff851288a8b1ce7c))

* style: format code with PEP 8 ([`eb0f702`](https://github.com/Riminder/hrflow-connectors/commit/eb0f702b04343d30aad7224204104646c82b7ece))

* style: clean doc ([`0007a02`](https://github.com/Riminder/hrflow-connectors/commit/0007a02c9a8db9352cc2f218242325bd0d00d3d0))

* style: correct docstrings ([`04f6ebc`](https://github.com/Riminder/hrflow-connectors/commit/04f6ebc4bd90d5474829bf590739bdb6a7fffd8c))

* style: reformat with black ([`7341ac7`](https://github.com/Riminder/hrflow-connectors/commit/7341ac78f6be2115506843222b7a07fd49c2753e))

* style: remove comment separator ([`54fc794`](https://github.com/Riminder/hrflow-connectors/commit/54fc7949c97a4e85f2bff6ae9002869ddd6c2df7))

* style: add docstrings to `connector` methods ([`56e8b21`](https://github.com/Riminder/hrflow-connectors/commit/56e8b2149be6e402f0432debd880ca59ce731eb8))

* style: reformat with black ([`24c3e73`](https://github.com/Riminder/hrflow-connectors/commit/24c3e735505dd8aaab709cf25e0cd43747346ef8))

* style: remaining withspace ([`8610c4e`](https://github.com/Riminder/hrflow-connectors/commit/8610c4eb13413bcb7168a64319413b0ee83df7b8))

* style: f -&gt; F ([`9ebc7cb`](https://github.com/Riminder/hrflow-connectors/commit/9ebc7cbb49b20895d47f0896ecd6dae71d320100))

* style: format with black ([`5c732a5`](https://github.com/Riminder/hrflow-connectors/commit/5c732a5a61823f78700fd93a17dbbc28cf69c2ad))

* style: refacto code ([`65d8eb0`](https://github.com/Riminder/hrflow-connectors/commit/65d8eb02e7e5428104a303a1bcbc9ccf7af561bc))

* style: remove useless parameter ([`643f846`](https://github.com/Riminder/hrflow-connectors/commit/643f8467fdc95525b06ff9a474b0aee121675541))

* style: add docstrings in `push` and `format` funcs ([`78a877b`](https://github.com/Riminder/hrflow-connectors/commit/78a877ba89f6ef5d15d0f0ad799fb157236155a3))

* style: change doctstring to fit the norme ([`94caf9a`](https://github.com/Riminder/hrflow-connectors/commit/94caf9ad6ee572e00eba184d3503d32a1632add4))

* style: change doctstring to fit the norme ([`9f0222a`](https://github.com/Riminder/hrflow-connectors/commit/9f0222a0fb85f5db80715385f52ce8279fe4254c))

* style: remove prints ([`1500097`](https://github.com/Riminder/hrflow-connectors/commit/15000976afe29941b203781dd2ee4088f59fb180))

* style: reformat with black ([`0cc050a`](https://github.com/Riminder/hrflow-connectors/commit/0cc050ae1781b941b508ca98951d1a551f80f4c9))

* style: update README ([`e6b6367`](https://github.com/Riminder/hrflow-connectors/commit/e6b63677e11f87299551be9c794ba2a658e3f113))

* style: reformat with black ([`fdd0c09`](https://github.com/Riminder/hrflow-connectors/commit/fdd0c097a0898d19da519baeaa2a95cd8515f717))

* style: reformat with black ([`40c9282`](https://github.com/Riminder/hrflow-connectors/commit/40c9282ba08fdf9870c0b723ffd2f1091dca2de8))

* style: format `test_connector.py` to PEP 8 ([`9919c34`](https://github.com/Riminder/hrflow-connectors/commit/9919c34d665cd7f0a9b57e54586f380e1a85b1f1))

* style: format `action.py` with PEP 8 ([`0d9eb79`](https://github.com/Riminder/hrflow-connectors/commit/0d9eb79683883f33106d2a5e29d37391bc975d5a))

* style: reformat with black ([`88a55d7`](https://github.com/Riminder/hrflow-connectors/commit/88a55d73e16d723ad07df6df2288f4f7fee4930a))

* style: format `test_connector.py` to PEP 8 ([`7eb135e`](https://github.com/Riminder/hrflow-connectors/commit/7eb135e4e10434a47c68d4023b94229fe70f6a6f))

* style: format `action.py` with PEP 8 ([`b1dbb58`](https://github.com/Riminder/hrflow-connectors/commit/b1dbb58ee885484ca0e6530c9a5b43c7c315510a))

* style: reformat with black ([`d745935`](https://github.com/Riminder/hrflow-connectors/commit/d74593574cfcb69ff601162a1e9e0547cff631fc))

* style: reformat with black ([`68a89ca`](https://github.com/Riminder/hrflow-connectors/commit/68a89cab951205d15bae1dcf5217abbe8e92912e))

* style: reformat with black ([`c606e57`](https://github.com/Riminder/hrflow-connectors/commit/c606e5717386340a21d4f9553e45dd0ea8720edc))

* style: change docstrings ([`8a73783`](https://github.com/Riminder/hrflow-connectors/commit/8a737837b0c92ca61fb83490501ddff4ae78b400))

* style: fix line spacing ([`2ee8d0a`](https://github.com/Riminder/hrflow-connectors/commit/2ee8d0a1f10c5efc9700dc095958ef524b5d8901))

* style: reformat with black ([`e5cc303`](https://github.com/Riminder/hrflow-connectors/commit/e5cc303eb921605b3cbf957882cbd776496ebc84))

* style: break lines in `format` description ([`5bf2669`](https://github.com/Riminder/hrflow-connectors/commit/5bf2669e80b8a96098e52f3f715c0a355ce01edc))

* style: reformat with black ([`72669d2`](https://github.com/Riminder/hrflow-connectors/commit/72669d239b172ef690f3699d6cf50bec762700db))

* style: reformat with `black` ([`3fe3663`](https://github.com/Riminder/hrflow-connectors/commit/3fe36630b9e1a7e18ea31bee2e755ea2648e00fa))

* style: reformat with`black` ([`357e21d`](https://github.com/Riminder/hrflow-connectors/commit/357e21d80d0079e200ea4ff7c52376d66f2b9217))

* style: add comments for clarity in explicit `sleeps` ([`8c15ea1`](https://github.com/Riminder/hrflow-connectors/commit/8c15ea138e3f338afa3bcc14ff4a6392f566d2e4))

* style: fixing some auto format exaggeration ([`4a2f804`](https://github.com/Riminder/hrflow-connectors/commit/4a2f8042b59e3dae9813189d8cedf9a64118e548))

* style: add comments to `pull` ([`7760a26`](https://github.com/Riminder/hrflow-connectors/commit/7760a2621b5245ddea2be01563302c3c279c5d17))

* style: add some `logging` details ([`ab7015c`](https://github.com/Riminder/hrflow-connectors/commit/ab7015c5f6a7d613bd9c28c7ea29a416e3cca2c1))

* style: add comment in  `pull` get job cards block ([`b827375`](https://github.com/Riminder/hrflow-connectors/commit/b827375f0f3d312dc91286791d279c59c7310568))

* style: reformat `JobsBuilder` with black ([`f55b97b`](https://github.com/Riminder/hrflow-connectors/commit/f55b97bc152e5105ad1a782facf69df1f0d83fd5))

* style: fix style Craigslist connector ([`72fc330`](https://github.com/Riminder/hrflow-connectors/commit/72fc330995f07189711f4cc1f87207f3e3256109))

* style: reformat `SmartCandidate` with `black` ([`0671da2`](https://github.com/Riminder/hrflow-connectors/commit/0671da25b2f295281dd1c1542d404a7c789cd0ac))

* style: make `format` subfunctions more explicit ([`3b94d69`](https://github.com/Riminder/hrflow-connectors/commit/3b94d690cdf36ac7b7c4b2ad941b37109c78ebd1))

* style: add comments to `pull` func ([`2d0446c`](https://github.com/Riminder/hrflow-connectors/commit/2d0446c063219eddd4a6f39804ab361fb4bc77d5))

* style: reformat `actions.py` with black ([`cb73846`](https://github.com/Riminder/hrflow-connectors/commit/cb7384648c4cf5b1b9e82c4bb51f0858b2251330))

* style: add comments in `pull` func ([`dc53809`](https://github.com/Riminder/hrflow-connectors/commit/dc53809f82a0be5c2a0d4962cb727813c1683b3b))

* style: enhance format with black ([`3de25e4`](https://github.com/Riminder/hrflow-connectors/commit/3de25e4edd5bf048aa56fca69c0124c62e32f618))

* style: add comments in `pull` function ([`72a68d1`](https://github.com/Riminder/hrflow-connectors/commit/72a68d1b3fd0ecb831c88792959125bdee6758d2))

* style: reformat `actions.py` in black ([`42ed404`](https://github.com/Riminder/hrflow-connectors/commit/42ed404c84f26ea9bd9d96bb5aa880eaea4dfcd6))

* style: reformat `test_smart_jobs.py` to black style ([`9aebd2c`](https://github.com/Riminder/hrflow-connectors/commit/9aebd2cb147a8ca03cdeef0c20dc4ee331739bd2))

* style: adding docstrings to `pull` and `format` functions ([`1a4c456`](https://github.com/Riminder/hrflow-connectors/commit/1a4c456b8a69e9cb37986c3713ee9401738cab02))

* style: add comment in `pull` finally pass block ([`f67e34e`](https://github.com/Riminder/hrflow-connectors/commit/f67e34e26171323d60ff2f131f1e4c8fc848ea69))

* style: reformat &#39;actions.py&#39; with black ([`ea2f697`](https://github.com/Riminder/hrflow-connectors/commit/ea2f697671544b40c29b8939cdc436fe2581fe82))

* style: reduce spaces  and reformat style ([`5642614`](https://github.com/Riminder/hrflow-connectors/commit/5642614dc721c9e14b94f829177b756c6cb7054a))

* style: reformat in black ([`12b5513`](https://github.com/Riminder/hrflow-connectors/commit/12b55138a2a2c167fd93f7ca2303cc2db4ca4644))

* style: rearrange docstrings ([`1fd93a5`](https://github.com/Riminder/hrflow-connectors/commit/1fd93a563c6b8cf13afd5303a1f0ceaf5b3146c3))

* style: reformat with black ([`908f18b`](https://github.com/Riminder/hrflow-connectors/commit/908f18b3637cef54a175b369020feb1f5101d2d7))

* style: remove last line return in code file ([`932ea16`](https://github.com/Riminder/hrflow-connectors/commit/932ea1618935e691619445301d0bc3a971a84643))

* style: formatting the code in black style ([`2f0e005`](https://github.com/Riminder/hrflow-connectors/commit/2f0e005cf908df00589629ff2483d16afffb9e59))

* style: format code to PEP 8 ([`455cd2b`](https://github.com/Riminder/hrflow-connectors/commit/455cd2b0959eccd02adf595c916723a0076a5f63))

* style: format all code src and tests (PEP 8) ([`38d74c2`](https://github.com/Riminder/hrflow-connectors/commit/38d74c287ececd01379296580cd5e232b9be7b0d))

* style: add comment in tests ([`30e1660`](https://github.com/Riminder/hrflow-connectors/commit/30e16607bc3dbf1ed6a9b6dddc7a2e6329762bbe))

* style: PEP 8 conf file for docs ([`4ea861b`](https://github.com/Riminder/hrflow-connectors/commit/4ea861b48beb67d7c4ede35b9169977b8cceb0d1))

### Test

* test: Update and add test for new root README update feature ([`5deed13`](https://github.com/Riminder/hrflow-connectors/commit/5deed1312e58a2fe2b70f307acda71317cd7d94a))

* test(fix): add hrflow_client in test_connector.py ([`6a89c14`](https://github.com/Riminder/hrflow-connectors/commit/6a89c145ce524279fc91a0c05c92765927d284e1))

* test(refacto): add auth config in workbale tests to replace credentials ([`517b305`](https://github.com/Riminder/hrflow-connectors/commit/517b3051ddb6495171c700ecd0fef7e5f3b19597))

* test: adapt tests to new schemas ([`5604df6`](https://github.com/Riminder/hrflow-connectors/commit/5604df67ba29a45395c5e6940e8b80d551329bc8))

* test: fix test for hydrate with parsing for cleaned_str ([`917b254`](https://github.com/Riminder/hrflow-connectors/commit/917b2543263a12e2163d95b390a3c0e5f706a6f2))

* test: add test for `Config` in `utils` ([`1e26ce5`](https://github.com/Riminder/hrflow-connectors/commit/1e26ce55f4c98dcba925ff9ba2d014e3e7e2badc))

* test: remove call HERE api key from credentials ([`06d41e8`](https://github.com/Riminder/hrflow-connectors/commit/06d41e84c27f0cc60777537738daba6ec43a9af0))

* test: fix typo in test_PushJobBaseAction ([`fa084e5`](https://github.com/Riminder/hrflow-connectors/commit/fa084e5e2c75e97ac449d65974b44bd2f90fdbc6))

* test: migrate board_keys to new boards in dev-demo ([`592149a`](https://github.com/Riminder/hrflow-connectors/commit/592149aba44ced8ed09aaf60afccba79ce5cdb83))

* test: `get_job` in `EventParser` ([`4e36e31`](https://github.com/Riminder/hrflow-connectors/commit/4e36e311456c3115b0679661451eb8b8f3bac722))

* test: `push` `CatchProfileAction` ([`0a4fb72`](https://github.com/Riminder/hrflow-connectors/commit/0a4fb722e1b4997ad31d79ce0357f2af3c33fb16))

* test: passed for both actions ([`bbbced2`](https://github.com/Riminder/hrflow-connectors/commit/bbbced2a8177a315b9b5378a35cf64e53846c90b))

* test: passed for new auth `XTaleezAuth` ([`aaa57df`](https://github.com/Riminder/hrflow-connectors/commit/aaa57dfe03a701127f5ba5b6d009adc4a2fa776e))

* test: passed for new auth `signin` ([`73753a0`](https://github.com/Riminder/hrflow-connectors/commit/73753a05a8b6c3955ed41dc04a227b0572888b93))

* test: passed for push profile ([`f825089`](https://github.com/Riminder/hrflow-connectors/commit/f8250899a5a8447dec952c6d6c13ac9ea6d11d4f))

* test: passed for pull_jobs ([`20fa9d9`](https://github.com/Riminder/hrflow-connectors/commit/20fa9d95d93d6e353c0eb697e542f8b55e7b9863))

* test: passed ([`919a7b0`](https://github.com/Riminder/hrflow-connectors/commit/919a7b0593abd3535382eb1ecce9d74f99286d4f))

* test: passed for push profile ([`26911b0`](https://github.com/Riminder/hrflow-connectors/commit/26911b08f3813894abc74715a893e729a654300a))

* test: change import in Crosstalent test ([`c95fc56`](https://github.com/Riminder/hrflow-connectors/commit/c95fc5665d1084d5d08d398aebdc61fd9027a308))

* test: `Connector` interface to call Crosstalent ([`270e639`](https://github.com/Riminder/hrflow-connectors/commit/270e6393628cb0f44f627937b1b5cc06824572db))

* test: `Auth` &amp; `OAuth2` failure ([`7d841b1`](https://github.com/Riminder/hrflow-connectors/commit/7d841b127a7a6f8f3d95b3f73190ddd9d6d681e8))

* test: get all reference failure &amp; less jobs ([`e6e9509`](https://github.com/Riminder/hrflow-connectors/commit/e6e95093783eaa33e82e663e716c2897955e466a))

* test: passed ([`d983ccc`](https://github.com/Riminder/hrflow-connectors/commit/d983ccc674728813e65503551e49783e43bcc768))

* test: `hydrate_job_with_parsing` failure ([`90a564a`](https://github.com/Riminder/hrflow-connectors/commit/90a564a02352574e786e6e1194e2f1952c98fbd5))

* test: check unarchiving failed &amp; parse empty text ([`0d2b5fb`](https://github.com/Riminder/hrflow-connectors/commit/0d2b5fbd879e3bdd723c9c6c19592cea1108fad2))

* test: validate with harvest api key ([`2366ef2`](https://github.com/Riminder/hrflow-connectors/commit/2366ef2281cd25000f81da0ad9d8546412783ba6))

* test: `check_reference_in_board` when ref is None ([`70700a3`](https://github.com/Riminder/hrflow-connectors/commit/70700a3b2fc32d7d6ea1ab206eb6324170df2280))

* test: `push` in `PullJobsAction` ([`92cbbf0`](https://github.com/Riminder/hrflow-connectors/commit/92cbbf025468f36888877f34bf1469ecbf3e4121))

* test: unauthorized fix ([`cfbd9c3`](https://github.com/Riminder/hrflow-connectors/commit/cfbd9c360c2ba21214b48c21f25e3400b02df57a))

* test: sap server failed ([`a173986`](https://github.com/Riminder/hrflow-connectors/commit/a173986745e5a5d9b33f0ff9e645a6383e0e7c65))

* test: execute for `PullJobsAction` ([`41125dd`](https://github.com/Riminder/hrflow-connectors/commit/41125dd4a47de28790297561be02223a06007376))

* test: add timedelta test with invalid int ([`9fdbce6`](https://github.com/Riminder/hrflow-connectors/commit/9fdbce6077cbbae05d7e89c6d1c88a98b47fe9aa))

* test: assert False in wrong datetime test ([`42bc985`](https://github.com/Riminder/hrflow-connectors/commit/42bc9851866b8aee54bc73c0eacce742e52b5dd9))

* test: assert False in wrong timedelta test ([`a0b2c4f`](https://github.com/Riminder/hrflow-connectors/commit/a0b2c4fa33ac4cac7d0619e091d393367c095bc2))

* test: invalide int for datetime converter ([`259c44d`](https://github.com/Riminder/hrflow-connectors/commit/259c44dbe87b641ae327c9800d9aeaeb48d5c905))

* test: timezone `hh:mm` for datetime converter ([`d6d9c62`](https://github.com/Riminder/hrflow-connectors/commit/d6d9c62d129cb8cb7d4d9c0982ec8dd9a3155fa7))

* test: disable propagation &amp; improve logger test ([`ba9b8dd`](https://github.com/Riminder/hrflow-connectors/commit/ba9b8ddff589d908a62c4c664f1d718fcd09f3a2))

* test: add test for logger ([`a7865c5`](https://github.com/Riminder/hrflow-connectors/commit/a7865c57361f7eea7dfa042d2a81f36a9b73cbf9))

* test: add `Connector_push_profile` ([`2537e7f`](https://github.com/Riminder/hrflow-connectors/commit/2537e7fb16e30782847ee516d3dfd0a7597798b9))

* test: add `Connector_push_job` ([`ce170ee`](https://github.com/Riminder/hrflow-connectors/commit/ce170ee2e02144faed10aeb1498c388072417766))

* test: add `Connector_pull_profiles` ([`fbb33da`](https://github.com/Riminder/hrflow-connectors/commit/fbb33da10a27ea330141a2184b9c49865edaff6e))

* test: add `Connector_pull_jobs` ([`39ad9d9`](https://github.com/Riminder/hrflow-connectors/commit/39ad9d9ee842448a166683787bef6014d042cfcf))

* test: add test for `PushAction` ([`1bb060e`](https://github.com/Riminder/hrflow-connectors/commit/1bb060e9ddf82590db32031eae5946f6f162b44d))

* test: add test for `PullAction` ([`3a4d08a`](https://github.com/Riminder/hrflow-connectors/commit/3a4d08ac723ef246d8db606984fa4ea0c638aba0))

* test: add timedelta test with invalid int ([`ecc5517`](https://github.com/Riminder/hrflow-connectors/commit/ecc5517dd74fa9f81d37d5d5e90d2ce503702768))

* test: assert False in wrong datetime test ([`71925d3`](https://github.com/Riminder/hrflow-connectors/commit/71925d3ea3b0db90dfbce07f769e1837bf614a07))

* test: assert False in wrong timedelta test ([`c75630b`](https://github.com/Riminder/hrflow-connectors/commit/c75630b061b724dfaa57d55df83c7139fd6485c0))

* test: invalide int for datetime converter ([`5bfacc3`](https://github.com/Riminder/hrflow-connectors/commit/5bfacc34397cb3c009824b0708f0d7f6732d8f5d))

* test: timezone `hh:mm` for datetime converter ([`2eb5ac6`](https://github.com/Riminder/hrflow-connectors/commit/2eb5ac6ead0ecc95612538e27280e09e09af4c01))

* test: disable propagation &amp; improve logger test ([`b5473c8`](https://github.com/Riminder/hrflow-connectors/commit/b5473c81097a5124526da02775fca8c2633567a2))

* test: passed for `PullJobs` ([`bc7e92c`](https://github.com/Riminder/hrflow-connectors/commit/bc7e92c7dc11312ef9f9ab73e975d91a8646b7f4))

* test: add test for logger ([`c237750`](https://github.com/Riminder/hrflow-connectors/commit/c23775066c1a45840857230a153fbf05090ad748))

* test: add `Connector_push_profile` ([`7c80315`](https://github.com/Riminder/hrflow-connectors/commit/7c803155ae71465f8e5071941989aa45667fed8f))

* test: add `Connector_push_job` ([`e532376`](https://github.com/Riminder/hrflow-connectors/commit/e5323765472c6c9d0ee9cf16ad863e7bb77ddfea))

* test: add `Connector_pull_profiles` ([`757cfe6`](https://github.com/Riminder/hrflow-connectors/commit/757cfe66b5f1b284197cb90394c367905e25e48a))

* test: add `Connector_pull_jobs` ([`ac57537`](https://github.com/Riminder/hrflow-connectors/commit/ac57537c17c2ee67873e26b93cb6546111b2d6df))

* test: add test for `PushAction` ([`69f40d7`](https://github.com/Riminder/hrflow-connectors/commit/69f40d7d59f4d70d1f48fc60a741b9fbf2743d76))

* test: add test for `PullAction` ([`a43e8ce`](https://github.com/Riminder/hrflow-connectors/commit/a43e8ce10c0c7829140a831f33931e8a0224b43d))

* test: typo: rename `test_EnrichProfile` ([`5ff4ae0`](https://github.com/Riminder/hrflow-connectors/commit/5ff4ae085d10e2ba123ebe73fd44f50a58b0a12c))

* test: `test_CareerJobs` passed for all params ([`3b606d6`](https://github.com/Riminder/hrflow-connectors/commit/3b606d69dcdbc2f7953946f5382f99a1dae35eb4))

* test: remove ROOT_PATH and use pytestconfig ([`87b4a7f`](https://github.com/Riminder/hrflow-connectors/commit/87b4a7fc46ddf3b6a664bdd4648642114d95b2cb))

* test: test carrerbuilder connector ([`00942a9`](https://github.com/Riminder/hrflow-connectors/commit/00942a9f3540b7e56426d282c88de00321856ba9))

* test: add profile example in SmartCandidate test ([`07ec7f7`](https://github.com/Riminder/hrflow-connectors/commit/07ec7f7b0174a74ca4e74d0e4972e23bc5e89b93))

* test: remove pandas `test_timedelta_converter` ([`1d56a7d`](https://github.com/Riminder/hrflow-connectors/commit/1d56a7db1035208f74314e4a502ac45f6ab86bba))

* test: remove pandas in `test_datetime_converter` ([`6790c83`](https://github.com/Riminder/hrflow-connectors/commit/6790c838b75332404e70f97c63454ddb886c177a))

* test: add `test_SmartToken` to `test_auth.py` ([`29c67be`](https://github.com/Riminder/hrflow-connectors/commit/29c67be30c525511e6a478678689cac0a06c49d5))

* test: pull generic XML &amp; test `job_list_xpath` ([`7e66379`](https://github.com/Riminder/hrflow-connectors/commit/7e66379a5c91132ccb73a18b532716616eca63b0))

* test: SmartRecruiters Push Profile ([`5b3d18f`](https://github.com/Riminder/hrflow-connectors/commit/5b3d18f71e5570526c34b803ca097bafa3a961b5))

* test: external format in Action ([`2fe7308`](https://github.com/Riminder/hrflow-connectors/commit/2fe730863216aa6283e384012884df0d22ecacd8))

* test: `generate_workflow_response` ([`3949206`](https://github.com/Riminder/hrflow-connectors/commit/3949206ffea534a606cbc205dd81d628a0094f04))

* test: Crosstalent Push Profile ([`907f95e`](https://github.com/Riminder/hrflow-connectors/commit/907f95e02c0b2e41bd13feff179cd9b4aae23626))

* test: `check_reference_in_board` ([`fdc6808`](https://github.com/Riminder/hrflow-connectors/commit/fdc6808302431bd07c1a48b206c67e0455c0d378))

* test: `hydrate...parsing` &amp; `get_all_references` ([`4b8fda0`](https://github.com/Riminder/hrflow-connectors/commit/4b8fda0cd716c0530f4a330e6cb56107e4ee89db))

* test: add test for apply_logics ([`d80b269`](https://github.com/Riminder/hrflow-connectors/commit/d80b2693488f52d77d2156123ac033654ad34497))

### Unknown

* doc: Update listing in root readme ([`99eaaa1`](https://github.com/Riminder/hrflow-connectors/commit/99eaaa1ee56de2e4e82cb7e5f75537660c71e550))

* doc: Minor doc update ([`e5ed364`](https://github.com/Riminder/hrflow-connectors/commit/e5ed36453349e81c8b9d1bf2bcf5a244c10aa7db))

* doc: Update root README with progress for all connectors ([`2ad2975`](https://github.com/Riminder/hrflow-connectors/commit/2ad29757d29ec5f96cca65bad730d3c74b71ae68))

* doc: Update DOCUMENTATION.md to reflect new addition of type parameter ([`b187227`](https://github.com/Riminder/hrflow-connectors/commit/b18722763964370bc18d1e07cd5bbe8ecd23beaa))

* doc: Update manifest with new type field data ([`2cdb146`](https://github.com/Riminder/hrflow-connectors/commit/2cdb14606ac684cf1b00bfb1d6a34f3eda4702d8))

* core: Run push hooks in Github Action CI/CD Workflow ([`7278126`](https://github.com/Riminder/hrflow-connectors/commit/7278126943108e20c2e4991cfda3fbc2091e7068))

* doc: Update action sections for all connectors ([`8a2a143`](https://github.com/Riminder/hrflow-connectors/commit/8a2a143ae9ae654b66f704ec5b4ce4ae572384f5))

* core: Use template files with global Jinja Environment ([`157a5a9`](https://github.com/Riminder/hrflow-connectors/commit/157a5a915f546a9b94de619247459735d6698f93))

* doc: Branch policy ([`a690c3f`](https://github.com/Riminder/hrflow-connectors/commit/a690c3f40a4df6cfcbebfec4b35677f7a62d1b3d))

* doc: Add missing listing for Salesforce push_profile action in README ([`66044c0`](https://github.com/Riminder/hrflow-connectors/commit/66044c066663134f7101658f5a84a5789b28ed45))

* Doc : Update connector documentation ([`12ced5c`](https://github.com/Riminder/hrflow-connectors/commit/12ced5cb274b11fa4488eabbde76bf5c850c790f))

* doc: Add section about new Connector copy feature in DOCUMENTATION.md ([`00940b4`](https://github.com/Riminder/hrflow-connectors/commit/00940b4c557cf3501bbef3fa1165590634a03ad4))

* core: Add feature allowing to create new connector based on another one ([`d1456fa`](https://github.com/Riminder/hrflow-connectors/commit/d1456faa95798d244ba8631c4e67a0b098f5d27c))

* minor: Correct achieved typo ([`6803ce3`](https://github.com/Riminder/hrflow-connectors/commit/6803ce34e922a4573309ac80a6226819e5b5b1d0))

* minor: Remove extra dealId parameters from Hubspot Warehouse test ([`9e69410`](https://github.com/Riminder/hrflow-connectors/commit/9e694104ca3230e286770744bdfc7b8ae63dbdb0))

* Update README.md to add salesforce link

Update README.md to add salesforce link ([`7e3235f`](https://github.com/Riminder/hrflow-connectors/commit/7e3235f84dc55f05bfabd9a28810448960b7609b))

* minor: Fix flake8 issues ([`5273a87`](https://github.com/Riminder/hrflow-connectors/commit/5273a8718d8ddf4b7a94af9cd98cfcfce46a642c))

* core: Add simple-salesforce package ([`4444333`](https://github.com/Riminder/hrflow-connectors/commit/4444333cb6d8e756960d6d08fbbf19bf3271c760))

* =core: Update POETRY version in Github Action following lock file change ([`151dbeb`](https://github.com/Riminder/hrflow-connectors/commit/151dbebe9e0acd99ac7247087297c3029f38a027))

* Copied legacy utils geolocation functions and data to utils (#151)

* Copied legacy utils geolocation functions and data to utils

* Document Utils Section

* Document Utils Section

---------

Co-authored-by: Moustapha Ebnou &lt;moustapha.ebnou@hrflow.ai&gt; ([`06c94e3`](https://github.com/Riminder/hrflow-connectors/commit/06c94e3ad73450404efb61ac11241ddd9177ad3a))

* Update README and roadmap connectors ([`eafa4db`](https://github.com/Riminder/hrflow-connectors/commit/eafa4dbe048f2a3e92b90d3b49e8e5f0bf81db78))

* 🚀 Recruitee : Update connector documentation for Release (#147) ([`df47d70`](https://github.com/Riminder/hrflow-connectors/commit/df47d70cabb40a307d60a10a28d8c93862c88026))

* Core: Add action name as Enum and action_type to Connector Action (#135)

* Core: Add action name as Enum and action_type to Connector Action

* :recycle: Major : Add action names and types validation &amp; updating all connectors ([`b81cd62`](https://github.com/Riminder/hrflow-connectors/commit/b81cd62827761be0ef8b3c7e1c88e04c4826b37c))

* Update Connectors RoadMap list and details ([`65b8240`](https://github.com/Riminder/hrflow-connectors/commit/65b8240a7a06a8fc15c38b9f24aaba892cb0be18))

* Roadmap : Add new connectors (ATS) to the roadmap ([`0c7985c`](https://github.com/Riminder/hrflow-connectors/commit/0c7985ce7e1a1386059451de8c8b746b9ea0cbd6))

* Roadmap : Added new planned connectors ([`038454e`](https://github.com/Riminder/hrflow-connectors/commit/038454e830a26a3e2f0ec2fcc0b45a639c55ef7c))

* core: Log start time and time spent reading and writing from warehouses ([`38a0b6e`](https://github.com/Riminder/hrflow-connectors/commit/38a0b6e50084fd9824a8788f1e69a9f5f5c4bdb4))

* feature: Activate PoleEmploi connector tests in CI/CD ([`05c84a4`](https://github.com/Riminder/hrflow-connectors/commit/05c84a406464e1682d55d356ef80700af343b95b))

* core: Fetch connectors secrets from AWS Secrets Manager ([`7dd4c19`](https://github.com/Riminder/hrflow-connectors/commit/7dd4c1974d5acf41c30d2ae2f5707a66177708eb))

* minor: Remove extra &amp;nbsp from CONTRIBUTING.md ([`3de25bf`](https://github.com/Riminder/hrflow-connectors/commit/3de25bfb4946f2edb0de8952ba720014f2fed84f))

* core: Put back install for baseline python version in Github Action workflow ([`2ff0ac2`](https://github.com/Riminder/hrflow-connectors/commit/2ff0ac2e0ce2ce0158809c72c8e2f335410a64c7))

* minor: Update docs ([`8d99474`](https://github.com/Riminder/hrflow-connectors/commit/8d994745658d9a4dc70f1f4fff0f82619eed32a4))

* doc: Update CONTRIBUTING.md with instructions about Nox sessions ([`218ba41`](https://github.com/Riminder/hrflow-connectors/commit/218ba41a7eaceab59bbc3090ab8e6d8b883e809c))

* core: Use nox suites in Github Action ([`8fa7428`](https://github.com/Riminder/hrflow-connectors/commit/8fa742821a83848450645e4e5c7a2e21fc90fdf4))

* core:Allow for python versions &gt;=3.7 and add nox test suite for core
tests ([`0242ae0`](https://github.com/Riminder/hrflow-connectors/commit/0242ae0908f83a1f0b4b6be6046db76b3c277e7c))

* minor: Update doc for connectors ([`c43d505`](https://github.com/Riminder/hrflow-connectors/commit/c43d505087ecdb0c29dc528eb199d9c20e2ac6f9))

* :bulb: New connector updated Doc Taleez ([`74df28e`](https://github.com/Riminder/hrflow-connectors/commit/74df28e8defe5511157a27377c660a497bae244a))

* Merge pull request #130 from Riminder/feature/connector-taleez

:rocket: New connector Taleez : Pull jobs from Taleez and Push profil… ([`91a395b`](https://github.com/Riminder/hrflow-connectors/commit/91a395ba6e1fbd522ddc9cae0200b3a471c59090))

* :rocket: New connector Taleez : Pull jobs from Taleez and Push profiles to Taleez ([`b4e89bb`](https://github.com/Riminder/hrflow-connectors/commit/b4e89bbf7754d7d1e37728c31ddb83adcadc74da))

* :bulb: Improving contributing documentation ([`7e59c0e`](https://github.com/Riminder/hrflow-connectors/commit/7e59c0ea48d1df7a1e6c043a747d99deb5f80b40))

* ✨ SAPSuccessFactors Connector documentation ([`15155e4`](https://github.com/Riminder/hrflow-connectors/commit/15155e4b34332e2a079636906014841d713749f2))

* Feature/update connectors documentation (#129)

* Update README.md

* Update README.md

* Update README.md

* Update README.md

* Update README.md

* Update DOCUMENTATION.MD

* Rename DOCUMENTATION.MD to  README.md ([`62e8412`](https://github.com/Riminder/hrflow-connectors/commit/62e84122f1403b34f5582807f2b3f244e894e1de))

* 📃Doc: added more extensive connector documentation for Adzuna and Bullhorn ([`d9f5c81`](https://github.com/Riminder/hrflow-connectors/commit/d9f5c8102ebe6e6948ea529fbec7d09cfaf79d66))

* 📚 Doc : Add TalentPool Custom Tab in Bullhorn display view ([`ceedb16`](https://github.com/Riminder/hrflow-connectors/commit/ceedb1660ca8e8192f6b2530b62e10dcddaf222d))

* 📚 Doc : Add TalentPool Custom Tab in Bullhorn display view ([`626c311`](https://github.com/Riminder/hrflow-connectors/commit/626c31173669b0424b5312c38187486b821a9f91))

* :memo: doc: update connectors documentation ([`3cec6af`](https://github.com/Riminder/hrflow-connectors/commit/3cec6afaca70d8ab727f88c0a546282743d58e0f))

* :memo: doc: update connectors documentation ([`1827c7b`](https://github.com/Riminder/hrflow-connectors/commit/1827c7b5cda5aad144db9f62516078ef73d42e2e))

* :memo: doc: update connectors documentation ([`82c874a`](https://github.com/Riminder/hrflow-connectors/commit/82c874afa4b6128fbc44df5338b603422bf1dc6f))

* :memo: doc: update connectors documentation ([`c159dd8`](https://github.com/Riminder/hrflow-connectors/commit/c159dd842f809e1f15bf6826f17ff5fd2333daba))

* :memo: Doc: Smartrecruiters, Cegid-Talentsoft and BreezyHR ([`48d7ea8`](https://github.com/Riminder/hrflow-connectors/commit/48d7ea8921c31d7a6433d43af7db61801368adab))

* ✍🏻 : Connector (BreezyHR) documentation update ([`bcf366f`](https://github.com/Riminder/hrflow-connectors/commit/bcf366f68cfcf080a11adcb1118f4cb730d9f950))

* :memo: Doc: Add direct links to source code in the connectors list ([`a1c51b7`](https://github.com/Riminder/hrflow-connectors/commit/a1c51b7ffff243eaee31158c0e815beb48e88365))

* :memo: Doc: Improving BreezyHR connector documentation (detailed version) ([`6117cf2`](https://github.com/Riminder/hrflow-connectors/commit/6117cf284562f5613d0a9d3f061e8f3575fc2239))

* :art: fix: correct DOCUMENTATION.MD for BreezyHR (#113)

* modify documentation ([`d91a762`](https://github.com/Riminder/hrflow-connectors/commit/d91a762f284829f1daa6fcf334177039d0b9cee0))

* Merge pull request #110 from Sprenger07/feature/fix-breezyhr-documentation

Update DOCUMENTATION.MD for BreezyHR ([`828d3b0`](https://github.com/Riminder/hrflow-connectors/commit/828d3b0530a9fc6774ed4cdff6df61d762eafa1c))

* Update DOCUMENTATION.MD ([`f57bb7d`](https://github.com/Riminder/hrflow-connectors/commit/f57bb7dbaae8996036c20f2ee45a0735477d2fbf))

* 📌 minor: typo fix in Readme ([`bb317a9`](https://github.com/Riminder/hrflow-connectors/commit/bb317a9c0a496e5e7fb14304c67903e16ea7a5e0))

* :memo: doc: Connectors releases and updates dates added and roadmap update ([`7c28889`](https://github.com/Riminder/hrflow-connectors/commit/7c28889b345f0551e6f49c2c2efcc90c28150e9b))

* :label: minor: Update manifest file with released connectors ([`83e5e77`](https://github.com/Riminder/hrflow-connectors/commit/83e5e77422189c236205460be2db4710ec7a23e1))

* :art: fix: Correct formatting issues and linting errors ([`2bfebae`](https://github.com/Riminder/hrflow-connectors/commit/2bfebaee967800987a4846cfa2376d268529001a))

* 🎉 Hubspot Connector : convert your HrFlow.ai Profiles to contacts/leads in Hubspot 

* Hubspot Connector v2.0

* Pull request review updates

Co-authored-by: Nedhir Ebnou &lt;nebnou@gmail.com&gt;
Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`fbc8eec`](https://github.com/Riminder/hrflow-connectors/commit/fbc8eecc22ee0ec709d1f006e9fa8426ba00ef47))

* 🎉 Waalaxy connector : push your leads and new connection from your sequence to HrFlow.ai

* Waalaxy Connector v1.0

* Waalaxy Connector v2.0: new event parser and ReadParameters

* changes according to the pull request review

* la doc

Co-authored-by: Nedhir Ebnou &lt;nebnou@gmail.com&gt;
Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`e87f93b`](https://github.com/Riminder/hrflow-connectors/commit/e87f93b6ad4d041624a9c26f967ee01f6b23cc5d))

* 🎉 Teamtailor connector 

* [SQUASH] ready for pull request (added field_type)

[SQUASH] removed is_auth field

This is a combination of 6 commits.

[ADD] init, schema, warehouse and connector

[MOD] connector and warehouse

[ADD] test-config.yaml

[MOD] added comments, removed test print()&#39;s

[MOD] style

[ADD] __init__.py

[SQUASH] ready for pull-request

[ADD] docs and README

[MOD] manifest.json

[MOD] added is_auth field to read and write parameters

[MOD] manifest.json

[MOD] Style

[MOD] removed is_auth field from write param

[MOD] manifest and docs

core: Define custom ParametersModel to force declaration of field_type

feat: Update tests and add tests for field_type validation

feat: Update existing warehouses with field_type

feat: Update manifest.json with new field_type

feat: Update DOCUMENTATION.md to reflect new ParametersModel and field_type constraints

added new field_auth type, and query filters for read action

* [ADD] docs

* Applied review advices

* Create DOCUMENTATION.md

* __nint__

Co-authored-by: Daniel Rosa &lt;daniel.rosa@hrflow.ai&gt;
Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`ee4d529`](https://github.com/Riminder/hrflow-connectors/commit/ee4d5290627f4cf48054121232bac222da62f538))

* 🎉 Connector Greenhouse

* greenhouse-connector-final-version

save

connecotr warehouse updated

greenhouse-connector-final-version

* field_type added to read/write param

* doc correction

Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`40db876`](https://github.com/Riminder/hrflow-connectors/commit/40db876fda898b507c9dbeee40114ea227ee862a))

* 🎉 Ceridian connector 

* [SQUASH] Ready for pull request (added field_type)

[SQUASH] Ready for pull-request

[ADD] Ceridian files, manifest and docs

[MOD] removed forgotten test print

[MOD] manifest

core: Define custom ParametersModel to force declaration of field_type

feat: Update tests and add tests for field_type validation

feat: Update existing warehouses with field_type

feat: Update manifest.json with new field_type

feat: Update DOCUMENTATION.md to reflect new ParametersModel and field_type constraints

added new field_type to read and write params

manifest and docs

* modified format signature

* Create DOCUMENTATION.md

* __init__

Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`d2d5790`](https://github.com/Riminder/hrflow-connectors/commit/d2d5790b9b0bf6b7d3250868458fe236cecede52))

* 🎉 Bullhorn connector : Sync profiles and jobs data between Bullhorn &lt;&gt; HrFlow.ai

* This is a combination of 5 commits.[PR]

first commit

switched to parsing warehouse

Push profiles, pull profiles parsing, pull profiles, pull jobs

added reference to profile

everything

* Create bullhorn_iFrame.md

* Create DOCUMENTATION.md

* code review

* __init__

Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`62ea4ad`](https://github.com/Riminder/hrflow-connectors/commit/62ea4ad6b8316f0d053a5b2fad735a70d5ebe515))

* 🎉 SAPsuccessfactors connector

* [SQUASH] ready for pull-request (added field_type)

first commit, edge cases need fixing

Missing: optional params

core: Define custom ParametersModel to force declaration of field_type

feat: Update tests and add tests for field_type validation

feat: Update existing warehouses with field_type

feat: Update manifest.json with new field_type

feat: Update DOCUMENTATION.md to reflect new ParametersModel and field_type constraints

Applied new Field_Type rules

added filed_type

* Applied review advices

* parsing_warehouse

* stash

* __init__

* snake_case

Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`70a3b9c`](https://github.com/Riminder/hrflow-connectors/commit/70a3b9c0fce44bfedadbc9eccff59679282bebe0))

* 🎉  BreezyHR connector

* add : BreezyHR connector

* add : BreezyHR connector wiht PEP8 format

Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`b0ae1e8`](https://github.com/Riminder/hrflow-connectors/commit/b0ae1e8f68c34485a0ded0e64d2d2e70d30fc512))

* 🎉  Workable  connector 

* add : Workable Connector

* add : Documentation.md

* Update warehouse.py

Co-authored-by: Leo-Ferretti-Hrflow &lt;113344020+Leo-Ferretti-Hrflow@users.noreply.github.com&gt;
Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`ff5a15b`](https://github.com/Riminder/hrflow-connectors/commit/ff5a15b31dd58708bcb6fadec7fbf9866cc30048))

* 🎉  Recruitee  connector

* Recruitee Connector v2.0

* changes according to pull request review

core test error fixed

changes according to the pull request review

docs and manifest

Co-authored-by: Nedhir Ebnou &lt;nebnou@gmail.com&gt;
Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`baa66da`](https://github.com/Riminder/hrflow-connectors/commit/baa66da8c2aa654df9f6dcb1afeeb9a00e7a6d10))

* 🎉  Adzuna connector

* Adzuna Connector v2.0

* changes according to the pull request review

Co-authored-by: Nedhir Ebnou &lt;nebnou@gmail.com&gt;
Co-authored-by: Moustapha Ebnou &lt;57711045+riquelme222@users.noreply.github.com&gt; ([`90912c3`](https://github.com/Riminder/hrflow-connectors/commit/90912c3f69c2761bc43f2407f7a0c94da43787e6))

* 🎉  Pole Emploi connector 

feature/connector-poleemploi: Pole Emploi Connector v2.0 ([`f3e3bc5`](https://github.com/Riminder/hrflow-connectors/commit/f3e3bc52e5d698c8053d18a1022b21b07399d84d))

* changes according to the pull request review ([`3803b53`](https://github.com/Riminder/hrflow-connectors/commit/3803b53558a13e2a1bc7ae6ea443ede38e5dc1c6))

* Pole Emploi Connector v2.0 ([`80cbbf5`](https://github.com/Riminder/hrflow-connectors/commit/80cbbf5f28b1c2116661c4cf63d2ad33e6db7f2c))

* core: Define custom ParametersModel to force declaration of field_type ([`37291f5`](https://github.com/Riminder/hrflow-connectors/commit/37291f56c711d41912ac035b9b88066873a6613d))

* Merge pull request #82 from Riminder/v2

V2 ([`db671c3`](https://github.com/Riminder/hrflow-connectors/commit/db671c376cde725463ef854c84942d07a21aed27))

* Merge branch &#39;master&#39; into v2 ([`41fc83f`](https://github.com/Riminder/hrflow-connectors/commit/41fc83f8185e2a7bab77032d303fdcc5d54adf75))

* major: Update github workflow to target master branch ([`697e626`](https://github.com/Riminder/hrflow-connectors/commit/697e626e253908090bf0373f115b367d217fe0d0))

* minor: Fix demo implementation of LocalJSONWarehouse write function ([`3a6dcb8`](https://github.com/Riminder/hrflow-connectors/commit/3a6dcb8d60d9dde317b9911811b2a2e1b2b560ef))

* major: Add incremental read mode to allow pulling to start from last read item ([`962d9ea`](https://github.com/Riminder/hrflow-connectors/commit/962d9ead493af7deec9f7377f5edb98ef13f175b))

* major: Add new backend feature to connectors with LocalJson first store ([`f3d0bc2`](https://github.com/Riminder/hrflow-connectors/commit/f3d0bc266d4d4743905e73ef540cb0de6261d1e3))

* bug: Add environment variables related to other branch feature/add-schedule-mode-with-lastuid for which tests fail if missing. This is a limitation imposed by running workflows with version in origin to avoid security vulnerabilities ([`cfc7ce1`](https://github.com/Riminder/hrflow-connectors/commit/cfc7ce1de52856db324b6596106f869ef438b687))

*  📝 Update connectors lists including authors&#39; names ([`a1ca0bd`](https://github.com/Riminder/hrflow-connectors/commit/a1ca0bd8e64dde94d516fc2a0278d0c32522e857))

* core: Add ipdb to dev dependencies ([`4ec7021`](https://github.com/Riminder/hrflow-connectors/commit/4ec702167235a0e849f6ae91589f604cdb29c33c))

* minor: Update docs and manifest ([`f7bc73f`](https://github.com/Riminder/hrflow-connectors/commit/f7bc73fce00a99bdb03da3512011c1971b8a432e))

* Update README.md ([`3bd14e8`](https://github.com/Riminder/hrflow-connectors/commit/3bd14e8270a2ba4deeea646fe728ebd16b165891))

* rename: Change Pull-&gt;Read and Push-&gt;Write for more naming consistency ([`5694bcb`](https://github.com/Riminder/hrflow-connectors/commit/5694bcbed9aabd2c7cd30795c00a25fe621726df))

* rename: Few variables renamed to be shorter ([`fff2f29`](https://github.com/Riminder/hrflow-connectors/commit/fff2f29739e4aa5820416a1a362992bd3ef6c031))

* Merge pull request #48 from Riminder/feature/alternative-proposition

Feature/alternative proposition ([`98e77dc`](https://github.com/Riminder/hrflow-connectors/commit/98e77dcc87391e2e513da002954b7ad38008b38d))

* minor: Few renamings ([`683e927`](https://github.com/Riminder/hrflow-connectors/commit/683e927b9ec58e64cf06cf22f5c8003083db8fa6))

* Name change pull -&gt; read, push -&gt; write ([`bdbf7ff`](https://github.com/Riminder/hrflow-connectors/commit/bdbf7ffab9870bef3e7923a84216364d50b26f65))

* major: Update poetry packaging information, update coding style, configure pre-commit hook, update core logic to add more metadata, add automatic doc generation ([`a8a3e73`](https://github.com/Riminder/hrflow-connectors/commit/a8a3e73b4afcc6947efaa070ac856143928a59f9))

* major: Alternative proposition for hrflow_connectors ([`7f5b29b`](https://github.com/Riminder/hrflow-connectors/commit/7f5b29b1c77940553e511746d596ba056ee57291))

* Update README.md ([`7d98aee`](https://github.com/Riminder/hrflow-connectors/commit/7d98aeef7b0449825be0cab135bdc6f3cbf74618))

* readme: Update the complete list of connectors on the Roadmap ([`71c7d8f`](https://github.com/Riminder/hrflow-connectors/commit/71c7d8fa6e7bdc3c4b6d88fc2bf095b0aaff7c6a))

* doc: last update for connectors ([`64b10b7`](https://github.com/Riminder/hrflow-connectors/commit/64b10b720058180bbd64f0d47026d8a234d33298))

* doc: add last updated date for connectors in README ([`500e92d`](https://github.com/Riminder/hrflow-connectors/commit/500e92d5c8e1da98e0097ebf53e394cb3f7a1b21))

* doc: add release date in connectors list in readme ([`cd7815c`](https://github.com/Riminder/hrflow-connectors/commit/cd7815c1d96fea3445b43f7cd2fab4161783c670))

* doc: delete duplicate in readme ([`9ecf668`](https://github.com/Riminder/hrflow-connectors/commit/9ecf66861629853b1f7d3e951b8c9cdba832b99b))

* Merge branch &#39;master&#39; of https://github.com/Riminder/hrflow-connectors ([`8ed5c4f`](https://github.com/Riminder/hrflow-connectors/commit/8ed5c4f62a1c720b96810da9950eb363288e256b))

* doc: update readme ([`7990bb7`](https://github.com/Riminder/hrflow-connectors/commit/7990bb7ffe06937234d127c41103dc25525fb19d))

* Merge pull request #46 from Riminder/dev

doc: add contributing.md ([`ae01d79`](https://github.com/Riminder/hrflow-connectors/commit/ae01d795da0399dd1e3957bd1528c49b81656634))

* Merge pull request #32 from Riminder/refacto/contributing

Doc: CONTRIBUTING.md ([`28b28e8`](https://github.com/Riminder/hrflow-connectors/commit/28b28e867989f5423584b33936b7e4184ab251e7))

* typo: update README ([`fb637bf`](https://github.com/Riminder/hrflow-connectors/commit/fb637bf326b0a40ec9f68a5ecaad25437f0a9772))

* Merge pull request #45 from Riminder/dev

Merge `dev` -&gt; `master` : version 1.1.0 ([`533ad28`](https://github.com/Riminder/hrflow-connectors/commit/533ad28b9c33b1fc9d2543dabdcd724ce5f5b63b))

* update version to 1.1.0 ([`e7eb2ab`](https://github.com/Riminder/hrflow-connectors/commit/e7eb2abc5bf33875cb5aea0756dfd9d11622438e))

* Merge pull request #44 from Riminder/architecture/v_1.1.0

Architecture/v 1.1.0 ([`eec0ee6`](https://github.com/Riminder/hrflow-connectors/commit/eec0ee6c526598655fb7c3184554207b3f5bfc99))

* doc: add type hints in auth ([`c18b152`](https://github.com/Riminder/hrflow-connectors/commit/c18b1524701cb5fd8032ddef38772e52e46b3a04))

* bug: remove company_id from test_push_profile ([`17ae682`](https://github.com/Riminder/hrflow-connectors/commit/17ae682139cbf4984476c186b630385139e6c715))

* doc: delete `before pushing the data` from `logics` and `format_function_name` docs ([`b48a650`](https://github.com/Riminder/hrflow-connectors/commit/b48a650c713dfab31df056ccc400b418ad9dc7f1))

* Merge pull request #43 from Riminder/feature/workable

Feature: connector to Workabe private endpoints ([`6678c80`](https://github.com/Riminder/hrflow-connectors/commit/6678c8016aef013626b50142114accbab9d98886))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into feature/workable ([`59d5f4e`](https://github.com/Riminder/hrflow-connectors/commit/59d5f4ee00c71c04c100de66b09223aedcc8ba0d))

* Merge pull request #42 from Riminder/refacto/credentials

Refacto: Change `credentials` to Config class ([`ccf14da`](https://github.com/Riminder/hrflow-connectors/commit/ccf14da44b83cf51815dd6426261cdadbe2e8c28))

* Merge pull request #41 from Riminder/architecture/schemas_pipeline

Architecture/schemas pipeline ([`9f0ed72`](https://github.com/Riminder/hrflow-connectors/commit/9f0ed72da37fc230e7a6a45752bd0b47a74c327f))

* doc: update bullhorn push_profile readme ([`538bb3b`](https://github.com/Riminder/hrflow-connectors/commit/538bb3b62630bc81d2b5802db9da379edcc0c851))

* doc: update directory architecture ([`9f8bcad`](https://github.com/Riminder/hrflow-connectors/commit/9f8bcad61cc659dd0795d23ddbc182504a2928e5))

* Merge branch &#39;refacto/contributing&#39; of https://github.com/Riminder/hrflow-connectors into refacto/contributing ([`ae3f555`](https://github.com/Riminder/hrflow-connectors/commit/ae3f55587cd713109fcdecdb58ea1137eed38071))

* doc: update contributing.md with recent schemas pipeline ([`075a4e4`](https://github.com/Riminder/hrflow-connectors/commit/075a4e406638347279f2517a576217759774a222))

* architecture: replace `Any` in TalentDataType with Basemodel ([`b1e086e`](https://github.com/Riminder/hrflow-connectors/commit/b1e086e112c402dca47c1b6cf4eb30af59fcb524))

* Merge branch &#39;architecture/schemas_pipeline&#39; of https://github.com/Riminder/hrflow-connectors into feature/workable ([`2860cd5`](https://github.com/Riminder/hrflow-connectors/commit/2860cd535fb62c331176e33c6dd435bd85b7b952))

* doc: addapt readmes to new schemas architecture ([`5deb7f9`](https://github.com/Riminder/hrflow-connectors/commit/5deb7f9af6396ae2b31c1be2c0b3e6d93abc7755))

* doc: add readme doc for ([`ab679df`](https://github.com/Riminder/hrflow-connectors/commit/ab679df03f23a3df3626c36e8c2ee85edd9e338a))

* doc: adapt readme to new changes ([`a0cc56d`](https://github.com/Riminder/hrflow-connectors/commit/a0cc56de35f3f5541a8a72ae1ebcdb6016db272a))

* Merge branch &#39;architecture/schemas_pipeline&#39; of https://github.com/Riminder/hrflow-connectors into feature/workable ([`5452cc3`](https://github.com/Riminder/hrflow-connectors/commit/5452cc327b5c3fd48eae06fefcedb23feb60dec9))

* doc: changge subdomin desc ([`cac998c`](https://github.com/Riminder/hrflow-connectors/commit/cac998c3d52755bbc3e9d5781783f2e0b6893639))

* Merge branch &#39;architecture/schemas_pipeline&#39; of https://github.com/Riminder/hrflow-connectors into feature/workable ([`7afcb9c`](https://github.com/Riminder/hrflow-connectors/commit/7afcb9c2ae60b4ad21b2ef8655e66ed98dd65ba5))

* typo: smartr format profile instituion -&gt; institution ([`2429e4b`](https://github.com/Riminder/hrflow-connectors/commit/2429e4b6941a4f93dc7d9b398e2f87ff58bc5471))

* Merge branch &#39;architecture/schemas_pipeline&#39; of https://github.com/Riminder/hrflow-connectors into feature/workable ([`221825a`](https://github.com/Riminder/hrflow-connectors/commit/221825a5938bc38fdd5c20c2d6e62d67b5d609a2))

* bug(fix): return Hrflowob model in hydrate_with_parsing if cleaned_str=&#34;&#34; ([`1e4e555`](https://github.com/Riminder/hrflow-connectors/commit/1e4e5553c89a789b828697b9eeacd93519c9dce6))

* Merge branch &#39;architecture/schemas_pipeline&#39; of https://github.com/Riminder/hrflow-connectors into feature/workable ([`c48bd7e`](https://github.com/Riminder/hrflow-connectors/commit/c48bd7ed3bce311e787a5b812a2c9979cb8aeb57))

* feate: update workable job schemas ([`7b32785`](https://github.com/Riminder/hrflow-connectors/commit/7b32785fd4b725b03fb2a1ca104d9fdab51692ea))

* clean: remove credentials fixture in conftest ([`1f47fe6`](https://github.com/Riminder/hrflow-connectors/commit/1f47fe616615e6100a0120651d594a311876aa6d))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into architecture/schemas_pipeline ([`8e7c7ab`](https://github.com/Riminder/hrflow-connectors/commit/8e7c7ab0271abbbfe5a737b8016d705cdd0cb540))

* doc: adapt types in docstrings to new objects basemodels ([`03a3ea5`](https://github.com/Riminder/hrflow-connectors/commit/03a3ea5fa00daabca6179d2e840693222ea69571))

* text: add hrflow_job model in format function switcher for XML feed ([`aa5a87b`](https://github.com/Riminder/hrflow-connectors/commit/aa5a87bb9c3374e4b59352a4070b83ec50e9d99f))

* tests(update): add hrflow basemodels in tests functions ([`394c9cf`](https://github.com/Riminder/hrflow-connectors/commit/394c9cf0870ab2d8f55a150f440bf8abb946b20e))

* Merge pull request #37 from Riminder/feature/teamtailor

Feature: Teamtailor connector ([`75108b8`](https://github.com/Riminder/hrflow-connectors/commit/75108b802b227885f9932f40681ed04f8bd0c1b9))

* Merge pull request #36 from Riminder/feature/bullhorn_migration

Feature/bullhorn migration ([`b9ca14d`](https://github.com/Riminder/hrflow-connectors/commit/b9ca14d560a6a11964b6e22332e3c98ec4051012))

* typo: Profile -&gt; HrflowProfile in push_profile typing ([`6ca21c6`](https://github.com/Riminder/hrflow-connectors/commit/6ca21c61627920a3cc802a80ce522a55941c1569))

* doc: add bullhorn endpoints ([`48ca779`](https://github.com/Riminder/hrflow-connectors/commit/48ca779fd50c8915dd88c819e416647505ceb03c))

* feature: add fonctionnal tests ([`96e3309`](https://github.com/Riminder/hrflow-connectors/commit/96e3309a48384542505e7436a9a466884e840b72))

* doc: add docstring ([`d00b356`](https://github.com/Riminder/hrflow-connectors/commit/d00b35664dfc24b3b40e3db641fcec06fc39768b))

* doc: change exemple with a more real one ([`dcae673`](https://github.com/Riminder/hrflow-connectors/commit/dcae67335cbe99b7f9368699b490717cdf3db851))

* Merge branch &#39;feature/schemas_pipeline&#39; of https://github.com/Riminder/hrflow-connectors into architecture/schemas_pipeline ([`dfb3eb5`](https://github.com/Riminder/hrflow-connectors/commit/dfb3eb5b9835cdc24edebbe297a973af052bca6a))

* doc: change README with bullhorn description ([`79b3d3e`](https://github.com/Riminder/hrflow-connectors/commit/79b3d3e0a51301fe0b733813001f573701e09abc))

* Merge branch &#39;test/devdemo&#39; of https://github.com/Riminder/hrflow-connectors into architecture/schemas_pipeline ([`1f45b59`](https://github.com/Riminder/hrflow-connectors/commit/1f45b59f67e0baee50fc21555d0d7406c9ce67a1))

* bug: remove metadatas to avoid indexing error ([`a2a0600`](https://github.com/Riminder/hrflow-connectors/commit/a2a060088c7260e1cd3bffb6f253b7ac5ea46205))

* Merge branch &#39;dev&#39; into feature/bullhorn_migration ([`bf3463a`](https://github.com/Riminder/hrflow-connectors/commit/bf3463ae1b5c118a117335245c200b4ea3e87a06))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into feature/schemas_pipeline ([`e585e38`](https://github.com/Riminder/hrflow-connectors/commit/e585e383460aae78e41418720e94b0810d884f48))

* bug: correct typo in `get_full_job` , pull_jobs_request -&gt; get_job_request ([`382d38d`](https://github.com/Riminder/hrflow-connectors/commit/382d38d9507efb60bc646f78e1491ebeb8d673ea))

* Merge pull request #35 from Riminder/refacto/breezyhr

Refacto: breezyhr ([`ebaaa2b`](https://github.com/Riminder/hrflow-connectors/commit/ebaaa2b759d22708a0df39f47b7f43ebfaa5e226))

* Merge pull request #38 from Riminder/schemas/hrflow

Feature: add hrflow objects schemas in utils ([`0572b9d`](https://github.com/Riminder/hrflow-connectors/commit/0572b9d0c83ab5e82c5ec86d9b9a0a8e2c1cf6d7))

* doc: add parameters typing objects in `send_request` func ([`863d0cf`](https://github.com/Riminder/hrflow-connectors/commit/863d0cf6fdbe657aaf5c689f36020793697b1f4a))

* doc: add type of returned object in send_request func ([`bc82e27`](https://github.com/Riminder/hrflow-connectors/commit/bc82e273bce7809d7bbc1865a3c4093013acfcf8))

* doc: add readmes for each action ([`00f265a`](https://github.com/Riminder/hrflow-connectors/commit/00f265af6b7633bd16cc3d1be4a1d13317fc7080))

* doc: add pull_jobs readme ([`58242a5`](https://github.com/Riminder/hrflow-connectors/commit/58242a51650d7efb596545904d0e30aba58423b1))

* doc: add readme for teamtailor connector ([`d17b63d`](https://github.com/Riminder/hrflow-connectors/commit/d17b63d9b7b4d8b7d750c3607371d1f59fccfb61))

* typo: resuest -&gt; request ([`c6b1abe`](https://github.com/Riminder/hrflow-connectors/commit/c6b1abe2141d9816ccb19f704f322440dec3c5f8))

* typo: resuest -&gt; request ([`8e1a658`](https://github.com/Riminder/hrflow-connectors/commit/8e1a658a6304e4e1b0718f76e3e26e74932f2df3))

* doc: fix typo in connector push profile docstring ([`983d58e`](https://github.com/Riminder/hrflow-connectors/commit/983d58ed497f05d83c7cfe13fac962cedf1a19dc))

* doc: add bullhorn to the root README.md ([`b0ca8ee`](https://github.com/Riminder/hrflow-connectors/commit/b0ca8eebae7c96ccabcd2fde3ac430755f18afdb))

* doc: add bullhorn push_profile.md ([`c19fb39`](https://github.com/Riminder/hrflow-connectors/commit/c19fb394767494cfb0d88153504c558581168ecf))

* doc: add bullhorn README.md ([`43c56d0`](https://github.com/Riminder/hrflow-connectors/commit/43c56d0abe7911712f76e2bbb4b013e0a357c5ad))

* refacto: add a `send_request` func in profile `push` to avoid repetition ([`a41c036`](https://github.com/Riminder/hrflow-connectors/commit/a41c036f6e252e4f434f6396285df38c608f480b))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into dev ([`79d4380`](https://github.com/Riminder/hrflow-connectors/commit/79d43809d6a72b50c3dbfee23fb32d61d69e440d))

* Merge pull request #34 from Riminder/feature/custome_error

Feature: add custom errors ([`4c72b5a`](https://github.com/Riminder/hrflow-connectors/commit/4c72b5a2f937306fed81ba7f7916f082720ca142))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into dev ([`0b25bb6`](https://github.com/Riminder/hrflow-connectors/commit/0b25bb69e91653756641296f134b9b5a62f4b584))

* Merge pull request #33 from Riminder/dev

Merge `dev` -&gt; `master` : version 1.0.2 ([`520f2bb`](https://github.com/Riminder/hrflow-connectors/commit/520f2bb31fbdb7c564e19df4595a520ae9ca1d2d))

* clean: remove CHANGELOG and use Github release ([`d35bc7b`](https://github.com/Riminder/hrflow-connectors/commit/d35bc7bf627f307beb570f5e48127d7b4dc9bc30))

* doc: fix python exemples indentation ([`ebfd030`](https://github.com/Riminder/hrflow-connectors/commit/ebfd030e07fa59b8a18b5d85e53e4336b89ca0f6))

* doc: add more python examples in the code contributions section ([`f08633f`](https://github.com/Riminder/hrflow-connectors/commit/f08633fe990ed4adb6e9d632ed54a717ad56e3ef))

* doc: fix link of `connectors` ([`528b55b`](https://github.com/Riminder/hrflow-connectors/commit/528b55b81aa96071c52ce7fda0216362eae46c57))

* doc: add more links to references: `utils`, `core` ([`744ed97`](https://github.com/Riminder/hrflow-connectors/commit/744ed973afac66be40b737378de6ed48a6c4d3bc))

* doc: correct duplicated title `code contributions` ([`063a381`](https://github.com/Riminder/hrflow-connectors/commit/063a381f8e19a35ed68819134a54f23f29eb1285))

* doc: add `TODO` in contributing documentation ([`d57d812`](https://github.com/Riminder/hrflow-connectors/commit/d57d812cb96cf2adccb5b1abb5d002821346fd3e))

* doc: fix typo auhtors -&gt;authors ([`6300cca`](https://github.com/Riminder/hrflow-connectors/commit/6300cca9280c429629836e69905e3d7ea5de48f5))

* doc: add example for `schemas.py` in `CONTRIBUTING.md` ([`3bb2de9`](https://github.com/Riminder/hrflow-connectors/commit/3bb2de98e99e57800a49c5678ab5dc5be1e073ad))

* doc: remove `ITALIC` format ([`c1fb042`](https://github.com/Riminder/hrflow-connectors/commit/c1fb0422cb8700e613d798c93c7415eff86d5ab1))

* Merge branch &#39;master&#39; of https://github.com/Riminder/hrflow-connectors into refacto/contributing ([`cc18ef7`](https://github.com/Riminder/hrflow-connectors/commit/cc18ef746d349269ad9d5d0aa4eaebd347a078ca))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into refacto/contributing ([`9480508`](https://github.com/Riminder/hrflow-connectors/commit/9480508b593b0e59525c92efe4739887b9a0aabc))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into dev ([`4870ad8`](https://github.com/Riminder/hrflow-connectors/commit/4870ad8d81cf8bbfad6286fde36bd5946e3b0c98))

* Update README.md ([`315d554`](https://github.com/Riminder/hrflow-connectors/commit/315d5549d47f21a368e6d662c9745853eb2f90af))

* Merge pull request #31 from Riminder/dev

Merge `dev` -&gt; `master` : version `1.0.0` ([`f05fe6b`](https://github.com/Riminder/hrflow-connectors/commit/f05fe6b3449bc733137cf22cc5c9e07eb6e24237))

* Merge pull request #29 from Riminder/refacto/rename_core_feature

Refacto: rename core class &amp; SAP Connector &amp; fix monster `PushJobAction` ([`2337c82`](https://github.com/Riminder/hrflow-connectors/commit/2337c821323d49fbb286c940d01b1688968d8bfd))

* Merge pull request #30 from Riminder/feature/refacto_action_documentation

Feature/refacto action documentation ([`e9d5b89`](https://github.com/Riminder/hrflow-connectors/commit/e9d5b89f13abafd3519f030cf1cd23eea7a01c23))

* refacto: change H1 title to bold in connector doc ([`4b9accf`](https://github.com/Riminder/hrflow-connectors/commit/4b9accf6ea156c7b7556dfc538410e8175362697))

* refacto: final update for today ([`30aec5f`](https://github.com/Riminder/hrflow-connectors/commit/30aec5f80985c28a16abdc5dab9f0cba1cd8ee2e))

* refacto: correct typos in all mds for actions ([`0972766`](https://github.com/Riminder/hrflow-connectors/commit/0972766ddd2757e9c4a35bcd2a8ff3baec03300b))

* refacto: SAP connector ([`668844e`](https://github.com/Riminder/hrflow-connectors/commit/668844e3a0b18dfaa44ab2d1b3dab3faed44a69a))

* refacto: change core `...Action` &gt; `...BaseAction` ([`1681957`](https://github.com/Riminder/hrflow-connectors/commit/1681957f42a490e7ea687a86802966d29230af97))

* refacto: logger in examples ([`7419743`](https://github.com/Riminder/hrflow-connectors/commit/741974375a100df57e1c5d866179ac540bb7ca76))

* refacto: removed all worfklow examples ([`d0d5a0c`](https://github.com/Riminder/hrflow-connectors/commit/d0d5a0ca4a0cfb41d03819b713a7a50480818473))

* refacto: update doc ([`102ce3e`](https://github.com/Riminder/hrflow-connectors/commit/102ce3e29b1b96169ffed0c6ec3c71817067bcfc))

* refacto: update crosstalent docs ([`d204f35`](https://github.com/Riminder/hrflow-connectors/commit/d204f353cf6e0ef2f6bf6497f1ec54c01d8597c3))

* refacto: remove workflow example and put endpoint table doc ([`6510ac6`](https://github.com/Riminder/hrflow-connectors/commit/6510ac6135ba56280951a98a00bea28d05bbaeec))

* doc contributing ([`f2f120b`](https://github.com/Riminder/hrflow-connectors/commit/f2f120bcfa089220677fbfdbe93bc6e27d0c5ed0))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into refacto/contributing ([`98307fc`](https://github.com/Riminder/hrflow-connectors/commit/98307fc3749f82ea5acc168b41cbf5580df8a4c5))

* refacto: rename `PushAction` -&gt; `PushBaseAction` ([`8c0acf0`](https://github.com/Riminder/hrflow-connectors/commit/8c0acf0ac6b06214952b169b1721f510be955e88))

* refacto: rename `PullAction` -&gt; `PullBaseAction` ([`d56640a`](https://github.com/Riminder/hrflow-connectors/commit/d56640a6f41340742288064b529475534365b43c))

* license ([`8dbc99a`](https://github.com/Riminder/hrflow-connectors/commit/8dbc99af6e93e4461069b3d7b76ea1f4b8c377a9))

* refacto: use `query_param_matcher` in `responses` ([`e509afd`](https://github.com/Riminder/hrflow-connectors/commit/e509afdcb69e175dc96bf40e4035d879cd7e9648))

* refacto: rename core `Action` -&gt; `BaseAction` ([`dd4b29d`](https://github.com/Riminder/hrflow-connectors/commit/dd4b29d6f66dc8940df4369f4d8ac8540daf89ae))

* Merge branch &#39;dev&#39; into refacto/rename_core_feature ([`1660f56`](https://github.com/Riminder/hrflow-connectors/commit/1660f5693dc84c41d2dc247f7650b02b5ec0f134))

* Merge pull request #28 from Riminder/feature/breezy_connector

Feature: add a Breezy.hr connector for pulling jobs and pushing profile ([`336b11e`](https://github.com/Riminder/hrflow-connectors/commit/336b11ee41ed1c8166079957439d9faa02d5ce44))

* doc: add company_name and position_id ([`225988c`](https://github.com/Riminder/hrflow-connectors/commit/225988cb6ba96dcdd0e9a001030a5812d1a78725))

* refacto: function name &amp; comment in `Breezyhr` ([`065a4dd`](https://github.com/Riminder/hrflow-connectors/commit/065a4dd1b39d969ccbcb3ae10bb7675443f28b06))

* refacto: tags in `format`for  `PushProfileAction` ([`1929d37`](https://github.com/Riminder/hrflow-connectors/commit/1929d379bb5d65398e55608e5d2c78a3275fb1ee))

* Merge branch &#39;feature/breezy_connector&#39; of https://github.com/Riminder/hrflow-connectors into feature/breezy_connector ([`a4c7ea4`](https://github.com/Riminder/hrflow-connectors/commit/a4c7ea4ba0cdc2328d848f9c731341561d03480a))

* refacto: rename `formated` -&gt; `formatted` ([`b4dd019`](https://github.com/Riminder/hrflow-connectors/commit/b4dd019eca8fc3e9972eebf398aec79f2b851efc))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into feature/breezy_connector ([`67012cd`](https://github.com/Riminder/hrflow-connectors/commit/67012cd7ffe652ef86efc5f4b6479aa710b377ba))

* bug: catch error code `COE0019` ([`345314d`](https://github.com/Riminder/hrflow-connectors/commit/345314d22b5b5d326c3319404c9c427c8b7f280e))

* Merge pull request #26 from Riminder/feature/test_monster

Feature/test monster ([`2c09f38`](https://github.com/Riminder/hrflow-connectors/commit/2c09f3887dd99f0ec26ed9459d5eb051ce18c11f))

* tests: `Job` &amp; `Board` in `utils` ([`35e7866`](https://github.com/Riminder/hrflow-connectors/commit/35e786635a29738604d7b4375af42791db145620))

* typo: documentation `EventParser` ([`f27f004`](https://github.com/Riminder/hrflow-connectors/commit/f27f004d9db0070b317e034b6bb86a37ef90d81e))

* debug: add more logging features ([`c54ca49`](https://github.com/Riminder/hrflow-connectors/commit/c54ca49ee7187aae3f06dd79e5b45de2434af28d))

* typo: correct `breezyhr` ([`acc8a69`](https://github.com/Riminder/hrflow-connectors/commit/acc8a691ad1bee24ec006920620bf4b0e48f958b))

* doc: add README.md for the connector ([`a112d2b`](https://github.com/Riminder/hrflow-connectors/commit/a112d2be52f83afb0839883a5314e8606d987bdc))

* Merge branch &#39;dev&#39; into feature/test_monster ([`c215623`](https://github.com/Riminder/hrflow-connectors/commit/c215623f1cd9c4f3d699d38c88c59dc5647f8da5))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into feature/breezy_connector ([`2974e64`](https://github.com/Riminder/hrflow-connectors/commit/2974e645a524bb061ced91e09640d945b630eaeb))

* bug: fix update profile request func ([`54c0dfc`](https://github.com/Riminder/hrflow-connectors/commit/54c0dfcbd43c43078d7f8ef11cdcb3b91ec25662))

* bug: add put profile in cse profile alredy exists ([`61cc97c`](https://github.com/Riminder/hrflow-connectors/commit/61cc97cb6c99e9340eae8e57f633d125005960de))

* Merge pull request #27 from Riminder/refacto/documentation_1.0.0

Documentation 1.0.0 ([`162f9ba`](https://github.com/Riminder/hrflow-connectors/commit/162f9baf321d913061852ca12a0158473b9e55b5))

* doc: add CONTRIBUTING.md ([`c1e46ef`](https://github.com/Riminder/hrflow-connectors/commit/c1e46efba66da24802bfb6eec304f6042ae9e7ea))

* doc: add monster in project README ([`9db05c9`](https://github.com/Riminder/hrflow-connectors/commit/9db05c93b9813d46af769f4ea8c4d2301d1ad303))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into feature/breezy_connector ([`14c562a`](https://github.com/Riminder/hrflow-connectors/commit/14c562ae0f895d08a1b153da9ba8229086e26949))

* create: Readme.md ([`65e7cbd`](https://github.com/Riminder/hrflow-connectors/commit/65e7cbdfc55fca52aad82f911db62905355efb48))

* Update README.md ([`b11475b`](https://github.com/Riminder/hrflow-connectors/commit/b11475bdaa4171042552c623be4e28dc9eeb2e45))

* Merge pull request #25 from Riminder/feature/taleez_connector

Feature: add Taleez connector ([`907e35a`](https://github.com/Riminder/hrflow-connectors/commit/907e35a908d15c4fbd6075aed6ab514ae79a9f3b))

* doc: update readme ([`7ccb8f7`](https://github.com/Riminder/hrflow-connectors/commit/7ccb8f77fef255c6ac93c4ed0ed4d89fbf7ed987))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into feature/breezy_connector ([`ce3c6c4`](https://github.com/Riminder/hrflow-connectors/commit/ce3c6c489e654c7dad5c35b04f9346f9b0582755))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into feature/taleez_connector ([`8acdca0`](https://github.com/Riminder/hrflow-connectors/commit/8acdca047bc88e2f9bee6741f430d212f77bea41))

* doc: add docstring in `OAuth2EmailPasswordBody` ([`5ba7c73`](https://github.com/Riminder/hrflow-connectors/commit/5ba7c73af17e6467da974d29a70714fdb2ca9faf))

* architecture: rename JobDestinationAction =&gt; PushJobAction ([`fff5cc6`](https://github.com/Riminder/hrflow-connectors/commit/fff5cc684fba042f7828a22a33512a2fce426bf5))

* doc: change README.md monster to integrate push_job ([`6e824d1`](https://github.com/Riminder/hrflow-connectors/commit/6e824d1f6bd15bc797e45ee3f608270bb1712295))

* doc: add documentation push_job ([`2bff766`](https://github.com/Riminder/hrflow-connectors/commit/2bff7663c1618bdbac2c4fd958d019e35c0d34a7))

* architecture: change documention catch_profile to integrate push_job ([`7d811e8`](https://github.com/Riminder/hrflow-connectors/commit/7d811e879f36b533865cb6bbd6b3a82a5c541412))

* sty:e remove huge linespaces ([`e9f0cfc`](https://github.com/Riminder/hrflow-connectors/commit/e9f0cfc98ddd496054e926d4f19ac1b3dcff4fc2))

* doc: update README with taleez connector ([`eb71d5d`](https://github.com/Riminder/hrflow-connectors/commit/eb71d5da7a9c7b92f09fec478c9b103c8434435b))

* Merge branch &#39;dev&#39; of https://github.com/Riminder/hrflow-connectors into feature/taleez_connector ([`abf47a6`](https://github.com/Riminder/hrflow-connectors/commit/abf47a614bd3befa513e37bcf38c615420b256f6))

* doc: add readme files for each action ([`c0ea677`](https://github.com/Riminder/hrflow-connectors/commit/c0ea6775fb7740232d6893bb5f3fa3466d5cd488))

* architecture: rename CatchProfileAction =&gt; CatchProfileAction and delete file_field field ([`65951d6`](https://github.com/Riminder/hrflow-connectors/commit/65951d6512998a73fe93215b3485b3bca8c8a6d7))

* doc: add README.md and docs for monster profile ([`edb4b52`](https://github.com/Riminder/hrflow-connectors/commit/edb4b528eaf894769165793d0f54fd146eb5470a))

* doc: add monster profile schemas.py ([`30f79f4`](https://github.com/Riminder/hrflow-connectors/commit/30f79f4c8dbe08b9a2b5a3b38ade33557b426516))

* typo: fix emoji in README ([`5b6a616`](https://github.com/Riminder/hrflow-connectors/commit/5b6a6169e4646f509755c4049db1b44a6c63f1f3))

* Merge pull request #24 from Riminder/feature/adapt_recent_connectors

Feat: Adapt last added connectors from version 0.2.0 to the architecture of version 1.0.0 ([`c883503`](https://github.com/Riminder/hrflow-connectors/commit/c8835034bba514c2c7a13cf046ccc5d364e1d2ef))

* doc: update readme.md with new connectors ([`298ef11`](https://github.com/Riminder/hrflow-connectors/commit/298ef11a5520d507168fca5fb26f8ac91056eb11))

* Merge branch &#39;feature/adapt_recent_connectors&#39; of https://github.com/Riminder/hrflow-connectors into feature/adapt_recent_connectors ([`feaf310`](https://github.com/Riminder/hrflow-connectors/commit/feaf310138867d2ff673979495e7526b2cd86a0b))

* update: adapt greenhouse connector to v1.0.0 ([`dcb0b63`](https://github.com/Riminder/hrflow-connectors/commit/dcb0b634f8da302fc3e353392b76d5ecbe586b65))

* update: adapt SAP connector to v1.0.0 ([`425fbeb`](https://github.com/Riminder/hrflow-connectors/commit/425fbeb1fb6c840679eda1f8b144515b89eee829))

* update: adapt Ceridian connector to v1.0.0 ([`d026227`](https://github.com/Riminder/hrflow-connectors/commit/d026227dcb64de7f50e8d4a4eee92e8bdca8be1d))

* update: adapt recruitee connector to v1.0.0 ([`956dcaf`](https://github.com/Riminder/hrflow-connectors/commit/956dcafbc2f600189c8520b350ec59406d0b503b))

* update: adapt workable connector ([`9e1468b`](https://github.com/Riminder/hrflow-connectors/commit/9e1468b1d6cac7f6e4e962894e411b800aa714da))

* Merge pull request #23 from Riminder/refacto/new_architecture_1.0.0

Refacto: Prepare to new architecture 1.0.0 ([`1cac9c6`](https://github.com/Riminder/hrflow-connectors/commit/1cac9c6314a638791616312765c943c20eed5768))

* Merge remote-tracking branch &#39;origin/dev&#39; into refacto/new_architecture_1.0.0 ([`b960194`](https://github.com/Riminder/hrflow-connectors/commit/b9601940831b99763a051adfd330bd19cd00d080))

* Merge pull request #22 from Riminder/feature/add_schemas

feat: add schemas models for connectors ([`d5bd669`](https://github.com/Riminder/hrflow-connectors/commit/d5bd66959fb3548dfe30e17059d7c64eb399dde9))

* doc: add new connector for ceridian dayforce ([`6af295c`](https://github.com/Riminder/hrflow-connectors/commit/6af295c48a0bd78a5cdae4a5b41473efdc7b05f9))

* Merge pull request #21 from Riminder/feature/ceridian_connector

Feature: Jobs Connector to Ceridian DayForce ([`b5c5c56`](https://github.com/Riminder/hrflow-connectors/commit/b5c5c564dd45ab5b09573755e78ee2d3da9e1a79))

* bug: fix location ([`c50b213`](https://github.com/Riminder/hrflow-connectors/commit/c50b2136faa6366d2d0cffcbfb3ce1202b1790a0))

* Merge pull request #20 from Riminder/feature/utils/lat_long

Feature: add function to get latitude and longitude of a city in utils module ([`b597b59`](https://github.com/Riminder/hrflow-connectors/commit/b597b5921b0be23af51c874ba188f563d18e4c83))

* doc: add README file ([`e10c1d8`](https://github.com/Riminder/hrflow-connectors/commit/e10c1d850fe9d2b96069ad185ca790d426d82b5b))

* clean: remove `spec.py` in Greenhouse ([`6034930`](https://github.com/Riminder/hrflow-connectors/commit/60349301b84edc05be81e50544d70e9d8ab19a4b))

* doc: add push profile action to recruitee ([`a94ff47`](https://github.com/Riminder/hrflow-connectors/commit/a94ff47b37c7d8ff4318e3b04310e639dbe66cc8))

* Merge pull request #19 from Riminder/feature/recruitee_push_profile

Feature: Add Recruitee push profile action ([`da904c2`](https://github.com/Riminder/hrflow-connectors/commit/da904c2e583c90649047f1810c164d23fd40a36f))

* doc: add readme for the connector ([`19a5f53`](https://github.com/Riminder/hrflow-connectors/commit/19a5f53217991549c2237e00697b29e51a296631))

* doc: add recruitee jobs connector ([`19de0d2`](https://github.com/Riminder/hrflow-connectors/commit/19de0d2a7bfa72fd28e0a24383ac13383b6c0b97))

* Merge pull request #18 from Riminder/feature/recruitee

Feature: add Recruitee connector for jobs feeds ([`456c246`](https://github.com/Riminder/hrflow-connectors/commit/456c246fc4957a273cdc1e6abba8b10818c69bf4))

* doc: add README file ([`0a8f16c`](https://github.com/Riminder/hrflow-connectors/commit/0a8f16c8c06354e062095a3d7d050681a5bf2316))

* architecture: add lat long data csv ([`36f427f`](https://github.com/Riminder/hrflow-connectors/commit/36f427f7dfe8a83428fb190fd749664a63eb6f8c))

* Merge pull request #17 from Riminder/feature/sap_successfactors_push_candidate

Feature: add connector to push profile in SAP SuccessFactors ([`4fc60d1`](https://github.com/Riminder/hrflow-connectors/commit/4fc60d1a33be7cb9ddbd6991e16defd9f16f3e41))

* update connectors in readme.md ([`958a04e`](https://github.com/Riminder/hrflow-connectors/commit/958a04eed0a2c9dc8cc24866ec99fc935fdbe6eb))

* Merge branch &#39;master&#39; of https://github.com/Riminder/hrflow-connectors into feature/sap_successfactors_push_candidate ([`4a96c56`](https://github.com/Riminder/hrflow-connectors/commit/4a96c569bcd905d811b77c57c42d5b70c853427b))

* architecture: readme for greenhouse ([`e03a226`](https://github.com/Riminder/hrflow-connectors/commit/e03a2265d325fe5dd87da2e538848ab1f4c6ec4c))

* Merge branch &#39;master&#39; of https://github.com/Riminder/hrflow-connectors into feature/sap_successfactors_push_candidate ([`8cd809a`](https://github.com/Riminder/hrflow-connectors/commit/8cd809a87e1588f36765c1b49af47dbaa448878c))

* Merge pull request #16 from Riminder/feature/greenhouse_push_profile

Feature: add greenhouse push profile connector ([`e48d045`](https://github.com/Riminder/hrflow-connectors/commit/e48d045252548bee27eec3b466183ef7f725a848))

* Merge pull request #13 from Riminder/feature/monster

Feature/monster ([`d06b20e`](https://github.com/Riminder/hrflow-connectors/commit/d06b20e1c4da603b544ccaf793052a30187a5267))

* typo: fix greenhouse name in README.md ([`a0a7ea3`](https://github.com/Riminder/hrflow-connectors/commit/a0a7ea3719fdfc8396d2cc2c4308927e151f5c79))

* rename: `subdomain` -&gt; `api_server` ([`c7c084b`](https://github.com/Riminder/hrflow-connectors/commit/c7c084b9ae4c4842d649d3d2a8c4cea0d8c1b0e6))

* refacto: reorganize tests for `Action` ([`f1a215d`](https://github.com/Riminder/hrflow-connectors/commit/f1a215d1b3d0d6e81d09186a3f1d0268f6b6ea5b))

* architecture: remove `ProfileDestinationAction` ([`1250965`](https://github.com/Riminder/hrflow-connectors/commit/1250965e97aa4e4296748a8a4b76a86419a9e65a))

* architecture: remove `BoardAction` ([`2c41939`](https://github.com/Riminder/hrflow-connectors/commit/2c4193925faf1d72d23213cec6c5f78b2699b04c))

* architecture: remove crawler connectors ([`8ea22a7`](https://github.com/Riminder/hrflow-connectors/commit/8ea22a7ea3d3832b40008ea036693d2cdfde75c8))

* architecture: rename Model -&gt; MonsterProfile in schemas.py ([`1daccdb`](https://github.com/Riminder/hrflow-connectors/commit/1daccdb9cf3e51070e018171720ce752d7622f90))

* feature: add schema monster profile ([`8dfd290`](https://github.com/Riminder/hrflow-connectors/commit/8dfd290df882754cd0796f6f6c4c1929bef342b5))

* feature: add description of each field ([`00c550d`](https://github.com/Riminder/hrflow-connectors/commit/00c550d427194ce8242634a03a44753afc1941aa))

* feature: add flatchr schema ([`8deaedb`](https://github.com/Riminder/hrflow-connectors/commit/8deaedb3d09cd40d02b3c2e92b3633dd9cd23d3c))

* Merge pull request #15 from Riminder/feature/workable_public_jobs

Feature: add connector for workable public endpoints job postings ([`5cef35c`](https://github.com/Riminder/hrflow-connectors/commit/5cef35cbe48a049b48b2ac934e203a82401f04a6))

* Merge pull request #14 from Riminder/feature/sap_success_factors_jobs

Feature: add SAP SuccessFactors connector for job requisitions ([`1b524a1`](https://github.com/Riminder/hrflow-connectors/commit/1b524a19c0be534fb322d7cb1ceb58b9d97e424d))

* Merge branch &#39;refacto/new_architecture_1.0.0&#39; of github.com:Riminder/hrflow-connectors into refacto/new_architecture_1.0.0 ([`a8e86f7`](https://github.com/Riminder/hrflow-connectors/commit/a8e86f7ed8b647acccb513befa36a11b94f846cb))

* refacto: `Connector` method to `staticmethod` ([`f529416`](https://github.com/Riminder/hrflow-connectors/commit/f529416162a2932d6893b2b0fbd68a2583b8ec7b))

* refacto: add `hrflow_client` in `Action` ([`8436061`](https://github.com/Riminder/hrflow-connectors/commit/843606102e5829c8d2baa72658e988f7c2d6d32b))

* refacto: `Connector` method to `staticmethod` ([`a97815b`](https://github.com/Riminder/hrflow-connectors/commit/a97815b369873a70289b22914ec0b6f8de822783))

* refacto: add `hrflow_client` in `Action` ([`b6e37d5`](https://github.com/Riminder/hrflow-connectors/commit/b6e37d5e1a4559af793fa649bcfd6084d6cefd27))

* feature: add test pull_profile monster ([`ac4f1e3`](https://github.com/Riminder/hrflow-connectors/commit/ac4f1e3e10ee0465aeec934543ded6456856d32d))

* doc: add README.md monster profile ([`6f70553`](https://github.com/Riminder/hrflow-connectors/commit/6f705531a9a1932a270081586278ddfdb0a903c7))

* feature: add pull_profile monster ([`0a7cbb2`](https://github.com/Riminder/hrflow-connectors/commit/0a7cbb274131ec9a113742f9b31b9b4543fc34c6))

* feature: add CatchProfile action ([`04985a1`](https://github.com/Riminder/hrflow-connectors/commit/04985a16e1ed83dea7957c4bf9995c7437527110))

* feat add `format` func ([`0f130f4`](https://github.com/Riminder/hrflow-connectors/commit/0f130f45746a613fe04030e02f3e28474bb2b06e))

* architecture: add monster profile ([`04eedff`](https://github.com/Riminder/hrflow-connectors/commit/04eedffdc46965dad9331fa040a89d41d3d5c2d2))

* doc: remove &#39;PullProfile&#39; from PushJob doc ([`3db3961`](https://github.com/Riminder/hrflow-connectors/commit/3db3961a963ba0597272877f801dea141b5c3afd))

* doc: add readme.md monster job ([`ad213b1`](https://github.com/Riminder/hrflow-connectors/commit/ad213b1041317ce8a449cf9cd744ca0a2088bf7e))

* feature: add EventParser.get_job() ([`c83e3b6`](https://github.com/Riminder/hrflow-connectors/commit/c83e3b6282a0e197e71435a6c487a023d97280ca))

* architecture: change XML auth by passing the credentials in the PushJob constructor ([`ddc2eb5`](https://github.com/Riminder/hrflow-connectors/commit/ddc2eb52364c5af04a85928bc76453e20819df99))

* feature: add JobDestinationAction ([`ed3f9dc`](https://github.com/Riminder/hrflow-connectors/commit/ed3f9dce52444a2e25c6ccf4cef7b1f64c704a01))

* Merge pull request #12 from Riminder/rename/connectors

Rename:  `GetAllJobs` -&gt; `CareerbuilderFeed`; `SmartJobs` -&gt; `GetAllJobs` ([`6be2295`](https://github.com/Riminder/hrflow-connectors/commit/6be229520657031a9dda5e6fc9680dc527d730ad))

* rename: carrerbuilder and smartrecruiters jobs connector ([`d8b28ec`](https://github.com/Riminder/hrflow-connectors/commit/d8b28ec5ff30bc9e21d9a5f7ab16fb8f9a329efd))

* rename: `SmartJobs` -&gt; `GetAllJobs` ([`cacca04`](https://github.com/Riminder/hrflow-connectors/commit/cacca0419cb2020c83b6480b8aeaec1bb5c5df8c))

* rename: `GetAllJobs` -&gt; `CareerBuilderFeed` ([`cb7ecb4`](https://github.com/Riminder/hrflow-connectors/commit/cb7ecb4a5613f17e9ca5aae0e26b9d0b435bb177))

* rename: `GetAllJobs` -&gt; `CareerBuilderFeed` ([`06a34d1`](https://github.com/Riminder/hrflow-connectors/commit/06a34d18dfe30678136d63b7780c86c19805dd39))

* Merge pull request #11 from Riminder/feature/greenhouse_job_board

Architecture: fix tags values : dict object -&gt; string ([`8da0809`](https://github.com/Riminder/hrflow-connectors/commit/8da08091c8a90b04868046b3ee1d681ac587d404))

* architecture: break tags values into string objects ([`49a89e6`](https://github.com/Riminder/hrflow-connectors/commit/49a89e6b766c7e421264c1880ea3fb662b0653f1))

* Merge pull request #5 from Riminder/feature/smartrecruiters_push_profile

Feature: add SmartRecruiters Push Profile ([`244e6da`](https://github.com/Riminder/hrflow-connectors/commit/244e6dad7631f3db9b5cf764086ea62e8bf869d3))

* typo: fix `format` documentation and comments ([`99b628f`](https://github.com/Riminder/hrflow-connectors/commit/99b628fa885ec5840db034901b3972f424e45eab))

* Merge pull request #8 from Riminder/feature/careerbuilder

Feature: Careerbuilder connector to pull job offers into HrFlow boards ([`b6d7535`](https://github.com/Riminder/hrflow-connectors/commit/b6d7535a3883ad377f4e5a129cc1ac0e9ff31557))

* Merge pull request #10 from Riminder/feature/greenhouse_job_board

Feature: Greenhouse connector to pull jobs into HrFlow.ai boards ([`929a61c`](https://github.com/Riminder/hrflow-connectors/commit/929a61c6cd749033061dce70a3c6f751a6a46d2c))

* debug: remove unnecessary logging ([`9234ecf`](https://github.com/Riminder/hrflow-connectors/commit/9234ecf03a438a6a36d8fc9644b4fef02706f4a8))

* feature: add Board and Job classes for PushJob ([`43166a0`](https://github.com/Riminder/hrflow-connectors/commit/43166a041aec9ae92f71b52a0d66b6cb28ab550a))

* feature: add auth for XML monster job ([`6f58944`](https://github.com/Riminder/hrflow-connectors/commit/6f5894491fdedcf635f1b1e4a90336055c63d544))

* typo: fix `education` in format ([`ffb0987`](https://github.com/Riminder/hrflow-connectors/commit/ffb0987c45588b6ee5c13fcd5fbd359b42e94bd8))

* doc: add example for `board_token` ([`5b97f76`](https://github.com/Riminder/hrflow-connectors/commit/5b97f769fb50a39a7366279c445d949672eca219))

* doc: add `README.md` for greenhouse jobs connector ([`28a4e47`](https://github.com/Riminder/hrflow-connectors/commit/28a4e47643a1cbaf4a831fe5a78c4118704ff80b))

* feature: add actions.py monster job ([`f2c0558`](https://github.com/Riminder/hrflow-connectors/commit/f2c05585a15400808f0d0d8ab65ca592214bace0))

* architecture: add monster jobs connector architecture ([`ce36d28`](https://github.com/Riminder/hrflow-connectors/commit/ce36d28732cfde0c0787ec073cacb119d059479f))

* architecture: rename hr_flow_profile -&gt; hrflow_profile &amp; XXXX -&gt; Undefined ([`efd6c31`](https://github.com/Riminder/hrflow-connectors/commit/efd6c31246ee22bd836d1b5eb9bc9bbbe1b1b668))

* debug: add logger under `StaleElement exception` ([`e1ca917`](https://github.com/Riminder/hrflow-connectors/commit/e1ca917c7813b38a52fbc7328f01cfab5cc8377d))

* refacto: `SmartProfile` -&gt; `PushProfile` ([`07d32d8`](https://github.com/Riminder/hrflow-connectors/commit/07d32d8da704d767976a5c4204c350b4c45e6447))

* refacto: `CareerBuilderFeed` -&gt; `GetAllJobs` ([`76faf8a`](https://github.com/Riminder/hrflow-connectors/commit/76faf8adb33f6d1f12d4143d5395b8a7d2a522f5))

* debug: add comment for exceptions in `pull` ([`c13c8b4`](https://github.com/Riminder/hrflow-connectors/commit/c13c8b4dcb6ea7a066ce6ead2f7b2caafde040d9))

* doc: remove duplication ([`d2c44c5`](https://github.com/Riminder/hrflow-connectors/commit/d2c44c5361eb61e6a272a5650cc7dbdba7681c78))

* doc: jobId -&gt; job_id ([`49eec2e`](https://github.com/Riminder/hrflow-connectors/commit/49eec2e68d6590f5218125a558077ccdc39781c8))

* typo: `SmartJobs` -&gt; `SmartProfile` ([`0d7d3f5`](https://github.com/Riminder/hrflow-connectors/commit/0d7d3f5fa3124ec60e028f4d30c142aab6a60915))

* doc: add `README.md` file for `SmartProfile` ([`39b8c82`](https://github.com/Riminder/hrflow-connectors/commit/39b8c82fa6314f8bc643dd6a9c53879359a03257))

* refacto: `SmartCandidate` --&gt; `SmartProfile` ([`2787c52`](https://github.com/Riminder/hrflow-connectors/commit/2787c52b160018930a35c3e418c92298fd4fd14f))

* Merge branch &#39;master&#39; of https://github.com/Riminder/hrflow-connectors into feature/smartrecruiters_push_profile ([`edded16`](https://github.com/Riminder/hrflow-connectors/commit/edded16112d72acad9342c75d10de115d3ca02bc))

* doc: add `README.md` for `CareerBuilderFeed` ([`9772c69`](https://github.com/Riminder/hrflow-connectors/commit/9772c6955c5968fdb240043807dc9cf9e9a5971b))

* refacto: `CareerJobs` --&gt; `CareerBuilderFeed` ([`d4e5d76`](https://github.com/Riminder/hrflow-connectors/commit/d4e5d7619bbe52fdb7339b2245ddcaa5aa5e99b9))

* Merge branch &#39;master&#39; of https://github.com/Riminder/hrflow-connectors into feature/careerbuilder ([`af13247`](https://github.com/Riminder/hrflow-connectors/commit/af132474551c6eb7dc2b2f94e1ad861bc96abe68))

* Merge pull request #9 from Riminder/Refacto/prepare_new_version

Refacto : prepare the package to a new version 0.2.0 ([`de041f5`](https://github.com/Riminder/hrflow-connectors/commit/de041f505f9e530814dbdd3cbf0bb3b2b77a09d1))

* doc: typo: remove `s` ([`51ea477`](https://github.com/Riminder/hrflow-connectors/commit/51ea477311c7a8d014af3d0c7b8de26343233fa2))

* doc: typo: spelling in README ([`5c1dbf1`](https://github.com/Riminder/hrflow-connectors/commit/5c1dbf170781e87ffddca400183d2f3bd908a356))

* doc: remove `:::` in CONTRIBUTING ([`4460a48`](https://github.com/Riminder/hrflow-connectors/commit/4460a481edaacd13d53969c96e9f10b3d1d2fa9f))

* doc: fix links to DOCUMENTATION in README ([`d445d8a`](https://github.com/Riminder/hrflow-connectors/commit/d445d8a82f86a77989873ea9b350072a756ee469))

* doc: add DOCUMENTATION ([`f1dd568`](https://github.com/Riminder/hrflow-connectors/commit/f1dd56825e795514e1c6ba7da8aa176442428269))

* doc: add doc for `logger` ([`31a5044`](https://github.com/Riminder/hrflow-connectors/commit/31a50442689662ef1ac2aa87186e963838ae2986))

* doc: wip: new README and add DOCUMENTATION.md ([`8d05c94`](https://github.com/Riminder/hrflow-connectors/commit/8d05c94c3a6da1a750c2544e84c23e9079ab4004))

* doc: move arrow in head of README ([`7b29b49`](https://github.com/Riminder/hrflow-connectors/commit/7b29b4919a0520ce87352a7e530463c751e578d5))

* doc: typo: `Crosstalent` -&gt; `Flatchr` ([`7e597cc`](https://github.com/Riminder/hrflow-connectors/commit/7e597ccc9afca34b5db791782f800faa56fddf31))

* refacto: move`get_all_references_from_stream` ([`a073955`](https://github.com/Riminder/hrflow-connectors/commit/a073955421f38ffc1cd0c17826b8d04ce9dffd9c))

* refacto: test: regroup fixtures ([`9ba3104`](https://github.com/Riminder/hrflow-connectors/commit/9ba3104c626d2ff91510e3dc42306b9a10ca2960))

* doc: add README for Flatchr ([`20f55da`](https://github.com/Riminder/hrflow-connectors/commit/20f55da44b794d34dc9f7312c79b1d6bf8edbac0))

* doc: README &amp; field descriptions for `PushProfile` ([`f4489a0`](https://github.com/Riminder/hrflow-connectors/commit/f4489a02c8bb9a387871d0d621595a73a68bbaf9))

* doc: add README for `XMLBoardAction` ([`3f71c97`](https://github.com/Riminder/hrflow-connectors/commit/3f71c9787a96f00b5b84399525dbc756c941adcb))

* doc: add README for SmartJobs ([`400da07`](https://github.com/Riminder/hrflow-connectors/commit/400da07bcf7fc50bf1a39b6e9dbaceb91513fec0))

* doc: add README &amp; field descriptions for Indeed ([`12c2b09`](https://github.com/Riminder/hrflow-connectors/commit/12c2b097a4db2c206573bd90aa2f77813f316345))

* doc: add README &amp; field descr for `Crosstalent` ([`5d788a0`](https://github.com/Riminder/hrflow-connectors/commit/5d788a02a66583dfa9d82e52dd71a8bd538a86f1))

* doc: README &amp; field descriptions for Craigslist ([`ac05801`](https://github.com/Riminder/hrflow-connectors/commit/ac05801acdb68c0d582856ef9d07b297bb967a4a))

* rename: `CraigslistJobs` -&gt; `CraigslistFeed` ([`787dc45`](https://github.com/Riminder/hrflow-connectors/commit/787dc454cb8d9032c09de498702a544a7374b1a3))

* doc: all Auth in `core/auth` ([`a2331bf`](https://github.com/Riminder/hrflow-connectors/commit/a2331bf7652b8540142ab53b48b90035a2b97837))

* refacto: XAPIKey with inheritance ([`8197f69`](https://github.com/Riminder/hrflow-connectors/commit/8197f6907a62545cace0c9d3b7600298b346d548))

* debug: add more logging features in `CareerJobs` ([`508a65c`](https://github.com/Riminder/hrflow-connectors/commit/508a65cdb2ecbd255263bf3e56ad6c6343c7709c))

* debug: enable logging ([`3a306f4`](https://github.com/Riminder/hrflow-connectors/commit/3a306f42a1af1705515a1a42826ebd3452d4cba4))

* refacto: rename `JobsBuilder` to `CareerJobs` ([`561b0ac`](https://github.com/Riminder/hrflow-connectors/commit/561b0ac983b4c9e1438e70e965cb70a8c07cb594))

* Merge pull request #7 from Riminder/feature/XMLBoard

Feature: add XMLBoardAction, datetime converters, external format func, refacto and config testsuites ([`41dc5be`](https://github.com/Riminder/hrflow-connectors/commit/41dc5be0120f771710c5a51047f9800c26827e53))

* Merge pull request #6 from Riminder/feature/Flatchr

Feature/flatchr ([`b6bee10`](https://github.com/Riminder/hrflow-connectors/commit/b6bee10e4741e3bb25782a73730dc347194f5b26))

* Merge branch &#39;master&#39; into feature/Flatchr ([`c582f88`](https://github.com/Riminder/hrflow-connectors/commit/c582f885aa3720faf7d5685c0c17d45318db4fad))

* Merge branch &#39;master&#39; into feature/smartrecruiters_push_profile ([`042b263`](https://github.com/Riminder/hrflow-connectors/commit/042b2632cd3216f2176a4c7882a08e71430f94b3))

* typo: fix typo in format ([`4983738`](https://github.com/Riminder/hrflow-connectors/commit/4983738cf0ae7c57ef74614469b5f0aa9b16e95b))

* Merge branch &#39;feature/smartrecruiters_push_profile&#39; of https://github.com/Riminder/hrflow-connectors into feature/smartrecruiters_push_profile ([`08ad944`](https://github.com/Riminder/hrflow-connectors/commit/08ad94408e00e606e9bffad2a3ffb2b389a2037c))

* Merge branch &#39;master&#39; of https://github.com/Riminder/hrflow-connectors ([`e8a7e41`](https://github.com/Riminder/hrflow-connectors/commit/e8a7e4102215f7ec449bbe5790138b7499294e13))

* Merge pull request #4 from Riminder/feature/Craigslist

Feature: Craigslist connector to pull job offers into HrFlow boards ([`72ef954`](https://github.com/Riminder/hrflow-connectors/commit/72ef9540b21e36ff9311fb7a482477d1830c6b91))

* Merge branch &#39;master&#39; into feature/Craigslist ([`c57c977`](https://github.com/Riminder/hrflow-connectors/commit/c57c977962e4c83924cb41da1124d73b4c7dbe7d))

* Merge branch &#39;master&#39; of https://github.com/Riminder/hrflow-connectors ([`60ee88a`](https://github.com/Riminder/hrflow-connectors/commit/60ee88adf3754dd7d653df5a0f9cb60a096182e1))

* Merge pull request #3 from Riminder/feature/SmartRecruiters

Feature: SmartRecruiters connector to pull jobs into HrFlow.ai boards ([`b8ce40a`](https://github.com/Riminder/hrflow-connectors/commit/b8ce40a2afedc0c4ea8383a1e3cd992587a00d90))

* remake: get location with one line in`format` `IndeedFeed` ([`c2fa126`](https://github.com/Riminder/hrflow-connectors/commit/c2fa12643f7bb3697b9f567d7566584a5e0e1cc0))

* refacto: test: remove ROOT_PATH &amp; use pytestconfig ([`04dd357`](https://github.com/Riminder/hrflow-connectors/commit/04dd357ab7bd7f0804dfe36b84a7c987d7e36de8))

* clean: remove pandas from dev dependencies ([`998f10a`](https://github.com/Riminder/hrflow-connectors/commit/998f10a5f49ba4843728893de60e5da8cc58b8ec))

* architecture: remove craiglist in test for conflict in PR ([`44c6b0b`](https://github.com/Riminder/hrflow-connectors/commit/44c6b0b0454d35d672ed29b3faee8924dea3fbfd))

* architecture: remove craiglist for conflict in PR ([`6aeb499`](https://github.com/Riminder/hrflow-connectors/commit/6aeb49905e06edeffe68cbeb483160341bc2ff11))

* feature: add auth with APIKEY tests ([`12c876a`](https://github.com/Riminder/hrflow-connectors/commit/12c876a6914b9c236a1f3ab4b0b9843956c58ffc))

* feature: add enrichment tests ([`2a05d21`](https://github.com/Riminder/hrflow-connectors/commit/2a05d210ddf833833345303ca0cb0aacbe133454))

* feature: add overwrite execute function in enrichment ([`22848ee`](https://github.com/Riminder/hrflow-connectors/commit/22848ee0247d294c6dab51232d0e683b1450fd4f))

* feature: add creation test ([`63c050e`](https://github.com/Riminder/hrflow-connectors/commit/63c050e0290774da83cc9e4cdc793b96197ea2cd))

* env: add webdriver-manager as a DEV dependency ([`422932a`](https://github.com/Riminder/hrflow-connectors/commit/422932ac15a2cf8647d798ab7ce5a093c5d5eed6))

* architecture: rename Flatchr -&gt; flatchr ([`96d9bea`](https://github.com/Riminder/hrflow-connectors/commit/96d9bea50e20b9ea1bc8acba6af3c0b1bf44c743))

* architecture: add test_enrichment_profile and test_push_profile Flatchr ([`202d3e2`](https://github.com/Riminder/hrflow-connectors/commit/202d3e2258da3dd6285af6c6ed642a104f5f7a90))

* rename: `references` -&gt; `reference` in `format` ([`45ed79e`](https://github.com/Riminder/hrflow-connectors/commit/45ed79e7e2ae7862497e2d70e8696e1a1356b170))

* feature: add EnrichProfile on Flatchr ([`d92082e`](https://github.com/Riminder/hrflow-connectors/commit/d92082e4424a7119a2e36a21be0cdf1c3c227f62))

* architecture: rename body -&gt; profile ([`8db1e57`](https://github.com/Riminder/hrflow-connectors/commit/8db1e57357f22a7c892688d4c6b74579f3279ecc))

* feature: add Flatchr PushProfile ([`d8538d3`](https://github.com/Riminder/hrflow-connectors/commit/d8538d34c4400eb343ddd39e12d4111061348817))

* architecure: rename AuthApiKey -&gt; APIKeyAuth ([`14bbf03`](https://github.com/Riminder/hrflow-connectors/commit/14bbf03671137da7f3a866276225af8cd24e5639))

* architecture: add new connector Flatchr ([`277bd35`](https://github.com/Riminder/hrflow-connectors/commit/277bd35ace58c6f84024de78eb25b99101103680))

* debug: catch  `WebDriverException` in case the subdomain is not valid ([`2f56837`](https://github.com/Riminder/hrflow-connectors/commit/2f5683770cf1bd483a2dc903fe82db64188dddfb))

* del: remove json dependency ([`70e8e0e`](https://github.com/Riminder/hrflow-connectors/commit/70e8e0ee91b6750513eff08867ed13e5203cdf90))

* refacto: change `pull` function to respect connectors convention ([`ad315b5`](https://github.com/Riminder/hrflow-connectors/commit/ad315b5e1b2bc21868e484c9340bd7fae0e10c81))

* typo: fix typos in docstrings ([`e03cdcf`](https://github.com/Riminder/hrflow-connectors/commit/e03cdcf2166799e391d04b60a095a53a998f2390))

* feat : add test file for `SmartJobs` ([`b20a1eb`](https://github.com/Riminder/hrflow-connectors/commit/b20a1ebc1fb37967e25a61eed1751d20239cae0e))

* add `pull` function to pull jobs with requests module ([`bfc2b9c`](https://github.com/Riminder/hrflow-connectors/commit/bfc2b9c8862300ab5b2776166128bddc5e9079bb))

* feat : Adding &#39;SmartJobs&#39; to pull jobs ([`268cf15`](https://github.com/Riminder/hrflow-connectors/commit/268cf15b015ab66d1f61329cff52214fb0f5cf30))

* Merge pull request #2 from Riminder/feature/crosstalent_push_profile

Feature: add Crosstalent Push profile ([`bc2d96d`](https://github.com/Riminder/hrflow-connectors/commit/bc2d96de6afdda7479467f4990b73eaf6e3787a2))

* architecture: rename `workflows` -&gt; `legacy` ([`13f00cf`](https://github.com/Riminder/hrflow-connectors/commit/13f00cf527af0a5630a395fc6381eebff0ef5ed6))

* Merge pull request #1 from Riminder/feature_indeed

Feature: indeed connector to pull job offers into HrFlow boards.. ([`210f5c5`](https://github.com/Riminder/hrflow-connectors/commit/210f5c54a0eba173c25712c8a0a8f238467f199e))

* ignore: add chromedriver to gitignore ([`590e52b`](https://github.com/Riminder/hrflow-connectors/commit/590e52bc7ba27f27ae6ab5ddf32fe7a423bd0110))

* edit: activate chrome_options in &#39;Crawler&#39; func ([`a199a9d`](https://github.com/Riminder/hrflow-connectors/commit/a199a9d4094db16cbe99fb1febe19fff43ff5a61))

* edit: rename &#39;GetAllJobs&#39; to &#39;IndeedFeed&#39; ([`f2d2a20`](https://github.com/Riminder/hrflow-connectors/commit/f2d2a200641cd94afae209bb68adffc800b09ffd))

* edit: update job type and salary in &#39;format&#39; ([`c2fa45b`](https://github.com/Riminder/hrflow-connectors/commit/c2fa45bb703cdfe5add0808b7d169ab8c0577770))

* edit: edit &#39;format&#39; description and summary ([`3aa05c3`](https://github.com/Riminder/hrflow-connectors/commit/3aa05c35f1998ae605453eac8703eba70375fffe))

* Perf: improve &#34;format&#34; by reducing try and except blocks in salary and jobType ([`d90aff5`](https://github.com/Riminder/hrflow-connectors/commit/d90aff5db8b3e6803bb5a004d6028d3bdd0e427b))

* Perf: improve parsing of job types in &#39;format&#39; func ([`5331df3`](https://github.com/Riminder/hrflow-connectors/commit/5331df3d0a32f216e727eddaa5edf7866ae20aad))

* Edit: in &#39;format&#39; get  name with find by class name instead of xPath ([`d96ac50`](https://github.com/Riminder/hrflow-connectors/commit/d96ac5056f8d411b7f6e336d59eb6e128c72e8f5))

* Edit: remove customized page limit ([`37fc77f`](https://github.com/Riminder/hrflow-connectors/commit/37fc77f91ce7af611bac04ab7a361c260645bf15))

* Feat: add &#39;executable_path as a parameter in the connector&#39; ([`335389b`](https://github.com/Riminder/hrflow-connectors/commit/335389b4d30d5173d81e1dd91b05740870d25ce4))

* Edit: modify the &#39;Crawler&#39; Class into a function inside &#39;GetALlJobs&#39; ([`b771404`](https://github.com/Riminder/hrflow-connectors/commit/b771404337419b490f3078e6d4dddfa48c6b22b8))

* Style: format code in black ([`70f9709`](https://github.com/Riminder/hrflow-connectors/commit/70f970926be86ad30b5cd21fa812f945e594c5d7))

* test : the test runs okay ([`b2416ca`](https://github.com/Riminder/hrflow-connectors/commit/b2416caec9c94c900394b6fafdf5a4047f278ab7))

* edit: fixed some selenium bugs, perf: improved ([`de1b87f`](https://github.com/Riminder/hrflow-connectors/commit/de1b87f3997f2d43af55bf100f9f3dc9342c0a61))

* refacto: rename `SourceDestinationAction` -&gt; `ProfileDestinationAction` ([`d4ebec8`](https://github.com/Riminder/hrflow-connectors/commit/d4ebec89c1b8df13d9640ded651f147e06d57e40))

* test :  get all jobs from indeed ([`dfae089`](https://github.com/Riminder/hrflow-connectors/commit/dfae089429b384a3a75f1d32947a9bdf57ed4310))

* edit: add function path to get url path, fix: some typos fixed ([`f3b1aa5`](https://github.com/Riminder/hrflow-connectors/commit/f3b1aa5422b882f1313955c89771638435588848))

* perf and style: avoid erronous info ([`6c559c3`](https://github.com/Riminder/hrflow-connectors/commit/6c559c338187475617dec98ca9bc9fb5a7636b2f))

* style : format the scripts with Black ([`0e97c32`](https://github.com/Riminder/hrflow-connectors/commit/0e97c32978767adbc9deb55b88535ca0af63328d))

* architecture: move some tests ([`39192df`](https://github.com/Riminder/hrflow-connectors/commit/39192dfa6bf96ed51a60a625c12ba2006e767aa3))

* typo: README python version ([`fc59bc6`](https://github.com/Riminder/hrflow-connectors/commit/fc59bc6be7a4f00be5e3e02dbc97e4dc353b3631))

* clean: remove doc config ([`fc82e69`](https://github.com/Riminder/hrflow-connectors/commit/fc82e69b188d642e74f2017e8f8090e17a3629bb))

* clean: remove docs folder ([`c1734fd`](https://github.com/Riminder/hrflow-connectors/commit/c1734fd3fbd9aa2ebaf06cd309abf2adfb9dc4dd))

* typo: rename `HTTPAction` -&gt; `HTTPStream` ([`2212f6f`](https://github.com/Riminder/hrflow-connectors/commit/2212f6f7c1c075133ca2ab99163af4c6aad202be))

* typo: rename `connect` to `format` ([`e38b326`](https://github.com/Riminder/hrflow-connectors/commit/e38b326b66d224ce39f2bb86f209d122f6b45d23))

* typo: rename of fixture in test_action ([`de11b0b`](https://github.com/Riminder/hrflow-connectors/commit/de11b0b84e13e9ab5fe46a60899e445662dee2dc))

* typo: rename test &#34;apply_filters&#34; &gt; &#34;apply_logics&#34; ([`3b2ef00`](https://github.com/Riminder/hrflow-connectors/commit/3b2ef0018f6831b5e4f8c2fdf5beee3fbfe033a1))

* refacto: OAuth2 object ([`78f2fa9`](https://github.com/Riminder/hrflow-connectors/commit/78f2fa913e65bc7e621898f71e63187e3d64f84f))

* refacto: HTTPAction ([`e7a2b56`](https://github.com/Riminder/hrflow-connectors/commit/e7a2b56acfc7ec2b9cdc8ad9c1b4b5f410eb6222))

* clean: remove unused import ([`05df9be`](https://github.com/Riminder/hrflow-connectors/commit/05df9bea355168b0aa6d994b6b9792329b244db4))

* init: add poetry conf and package setup ([`d7157c6`](https://github.com/Riminder/hrflow-connectors/commit/d7157c67352b05edd34535be94f30304f9111058))

* remove: ./ExampleConnector from root ([`d4fcff7`](https://github.com/Riminder/hrflow-connectors/commit/d4fcff748d02b044841e8ec5f94247c02a7773f9))

* update: .gitignore ([`a8114d9`](https://github.com/Riminder/hrflow-connectors/commit/a8114d990b24e5bf513916343cedbea7730bf882))

* move: .ExampleConnector to workflows directory ([`5a99773`](https://github.com/Riminder/hrflow-connectors/commit/5a99773af1567fa5fdc8a38f6280bf35b1d60edd))

* move: all old workflows in subdir ([`d292d47`](https://github.com/Riminder/hrflow-connectors/commit/d292d47d18aa104bc048d42e79002bcf3a93380b))

* Update push_jobs_from_hrflow_into_sendgrid.py ([`e4fbd01`](https://github.com/Riminder/hrflow-connectors/commit/e4fbd010567412aa22983d94d270010a1474233b))

* Update push_jobs_from_hrflow_into_sendgrid.py ([`c516701`](https://github.com/Riminder/hrflow-connectors/commit/c516701c2ceee85b3822bd80469e1865710316c7))

* Update push_jobs_from_hrflow_into_sendgrid.py ([`e072eeb`](https://github.com/Riminder/hrflow-connectors/commit/e072eeb1f72e461a92085a53652e8f60f6f2f600))

* Update push_jobs_from_hrflow_into_sendgrid.py ([`8b81e84`](https://github.com/Riminder/hrflow-connectors/commit/8b81e843d945dceddff5362c6e3985debb9f0285))

* Update push_jobs_from_hrflow_into_sendgrid.py ([`a2ef9fb`](https://github.com/Riminder/hrflow-connectors/commit/a2ef9fbbf6bb41ffbd304fb5ed284d8738dd310c))

* Update push_jobs_from_hrflow_into_sendgrid.py ([`d2cc774`](https://github.com/Riminder/hrflow-connectors/commit/d2cc774779191d143f41d01ce99feb6ba3f89528))

* workflow code ([`662eb88`](https://github.com/Riminder/hrflow-connectors/commit/662eb8827e6c83c33f8112935b7cba7abab7362e))

* workflow code ([`65d3be1`](https://github.com/Riminder/hrflow-connectors/commit/65d3be195437b53b9ac84559a735d5536a2e11e8))

* Read me ([`84260f0`](https://github.com/Riminder/hrflow-connectors/commit/84260f0338ebe741feaf38e1438ebec7881c1dd5))

* License ([`81e291b`](https://github.com/Riminder/hrflow-connectors/commit/81e291bb1cc01e833c6cf36fb1fc61a695da4059))

* SendGrid workflow

Periodically send relevant job offers to each candidate ([`3d23392`](https://github.com/Riminder/hrflow-connectors/commit/3d23392fb89d3cf7a55431dce82e72cb5b9b7b9f))

* ADD talentsoft connector ([`ce7b895`](https://github.com/Riminder/hrflow-connectors/commit/ce7b8951d18f1a3fcf7e724c4f6fd3fb202f3a17))

* docstring Mailchimp

docstring Mailchimp ([`e064353`](https://github.com/Riminder/hrflow-connectors/commit/e064353ae730b1a285e00924cbf0c77dcf09334d))

* mailchimp connector ([`2652ff2`](https://github.com/Riminder/hrflow-connectors/commit/2652ff2acc5cd021a94839fdd54ac2329d85c396))

* Refactor Crosstalent connector ([`d7fc663`](https://github.com/Riminder/hrflow-connectors/commit/d7fc663918121109ab2b5c2c1480b178bd547054))

* Refactor Staffme connector ([`57a4230`](https://github.com/Riminder/hrflow-connectors/commit/57a423059a0f450543bf042e8d3e11c7a6e136ce))

* Refactor Laponi connector ([`92dd3c4`](https://github.com/Riminder/hrflow-connectors/commit/92dd3c48fdbcd749d00b3d8041e92f693570c322))

* ADD README.md to StaffMe connector ([`af198bf`](https://github.com/Riminder/hrflow-connectors/commit/af198bfd5119f70beccd3eb2452359cce28a9f1e))

* ADD README.md to Laponi connector ([`113a858`](https://github.com/Riminder/hrflow-connectors/commit/113a858a36e135dde2b32c33cf5ce33710404bde))

* ADD README.md to Digitalrecruiters connector ([`18cd817`](https://github.com/Riminder/hrflow-connectors/commit/18cd8177f5d4312f7e5eaf33c48eecbe9cb2b8c4))

* ADD Digitalrecruiters / Laponi / Staffme connectors ([`4463e86`](https://github.com/Riminder/hrflow-connectors/commit/4463e863e5d1fd320e4db89f636551493fb65832))

* Crosstalent connector : Push profile from hrflow into crosstalent ([`00c8b61`](https://github.com/Riminder/hrflow-connectors/commit/00c8b614e8564b569b1e348bf3da4ffc868cf376))

* update tutorials ([`0be428e`](https://github.com/Riminder/hrflow-connectors/commit/0be428eca6170509f4d5ac39f41e3cb2850990e9))

* add annotations twilio+craigslist ([`eb86120`](https://github.com/Riminder/hrflow-connectors/commit/eb861205dab6a667a698df142888450c19fa00b5))

* integrate Crosstalent as a Destination ([`8b403ac`](https://github.com/Riminder/hrflow-connectors/commit/8b403ac27a85e2d14c1a0216b7df3a3fde25449d))

* refactor cragslist ([`e29fbbf`](https://github.com/Riminder/hrflow-connectors/commit/e29fbbfec16ed9e00c08703bd70328a29c269321))

* refactor craigslist connector ([`2f710d2`](https://github.com/Riminder/hrflow-connectors/commit/2f710d2dcd8d9c8adff7df8cbd15610de1cb9a52))

* Rename pull_jobs_from_graigslist_into_hrflow.py to pull_jobs_from_craigslist_into_hrflow.py ([`20cbe3e`](https://github.com/Riminder/hrflow-connectors/commit/20cbe3e931d0645ca2315d49e6ff182fa19eb768))

* Update README.md ([`878ed17`](https://github.com/Riminder/hrflow-connectors/commit/878ed17c8000772bdef6fc676f1351ea347110af))

* Twilio source Connector example + Code refactoring ([`764b0d1`](https://github.com/Riminder/hrflow-connectors/commit/764b0d116d931d62b169a8076fb3682f84c2c578))

* refactor code + renaming ([`f603f8c`](https://github.com/Riminder/hrflow-connectors/commit/f603f8cdadc3158856ed42e266649b4bb775b95b))

* Create pull_jobs_from_graigslist_into_hrflow.py

adding pull_jobs_from_graigslist_into_hrflow.py ([`a43da38`](https://github.com/Riminder/hrflow-connectors/commit/a43da386bb0bc3b4b19b4e7b1878495f8882e9e9))

* Add README for smartrecruiters connector ([`3c473e6`](https://github.com/Riminder/hrflow-connectors/commit/3c473e6bca76b861bb3790baf3ef1ccd0090b892))

* Update README.md ([`5696a44`](https://github.com/Riminder/hrflow-connectors/commit/5696a44c097c7f2e8ce26a2e5c9eb8f73d095e4c))

* Add README for smartrecruiters connector ([`c02d56a`](https://github.com/Riminder/hrflow-connectors/commit/c02d56a766b49785849cd4b1a3f77adf46e70233))

* Add README for smartrecruiters connector ([`1234251`](https://github.com/Riminder/hrflow-connectors/commit/123425124298c216cfd1967dd386ae971bf92d50))

* Add README for smartrecruiters connector ([`145610e`](https://github.com/Riminder/hrflow-connectors/commit/145610ed176ea61894f5dd7e19eabb107a8fb870))

* update Readme.md ([`a9ffc19`](https://github.com/Riminder/hrflow-connectors/commit/a9ffc19428a83579cdd01bcb5e6306d4836cf67b))

* Merge branch &#39;master&#39; of github.com:Riminder/hrflow-connectors ([`1c304e4`](https://github.com/Riminder/hrflow-connectors/commit/1c304e4b44aaedd5cc27bf3610fb4af890d43d65))

* update Readme.md ([`c0c8aeb`](https://github.com/Riminder/hrflow-connectors/commit/c0c8aebedaab327e0608b623630e411c31ce18cd))

* Update README.md ([`52deb60`](https://github.com/Riminder/hrflow-connectors/commit/52deb60792544041b62b710711f7f161cd24968a))

* first commit ([`a504157`](https://github.com/Riminder/hrflow-connectors/commit/a504157375327c74b7a10fd096d76b1f309b4952))

* first commit ([`f6ff33d`](https://github.com/Riminder/hrflow-connectors/commit/f6ff33d3099f4823c33244390cd98628c718ae43))
