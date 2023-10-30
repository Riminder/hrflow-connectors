# CHANGELOG



## v4.1.0 (2023-10-30)

### Documentation

* docs: correct picture of data exchange for salesforce connector (#189)

Co-authored-by: the-forest-tree &lt;the-forest-tree@hrflow.ai&gt; ([`43734a9`](https://github.com/Riminder/hrflow-connectors/commit/43734a9aa36fd9e8a2052e8864fe614fae657109))

### Feature

* feat: new action types
This is an empty commit to trigger new release of hrflow-connectors
Original PR was merged using a Squash which bypassed the semantic-version
rule of releasing depending on commit messages ([`16844d9`](https://github.com/Riminder/hrflow-connectors/commit/16844d97b8e34db04d88b662083af8b30f2c3ea7))

### Unknown

* Adding new actions &#39;pull_application_list&#39; and &#39;push_score_list&#39;.  (#184)

* Adding new actions &#39;pull_application_list&#39; and &#39;push_score_list&#39;. They will be used to sync applications (profiles, jobs, statuses) and synchronize scores from HrFlow.ai to a target warehouse

* style: apply black formatting

* test: add new pull_application_list to coherence tests

* fix: use random key for backend test to avoid failure in ci
It seems that when running multiple ci run in the same time
race condition can occur and one test can find the result of another
running in the same time

---------

Co-authored-by: thomas &lt;thomas.zhu@hrflow.ai&gt;
Co-authored-by: the-forest-tree &lt;the-forest-tree@hrflow.ai&gt; ([`df7d387`](https://github.com/Riminder/hrflow-connectors/commit/df7d3874bee3bf9d991f2d45b991330684ff6c0f))


## v4.0.0 (2023-10-30)

### Breaking

* feat: connector must know contribute a logo

BREAKING CHANGE: Each connector is now expected to have
a logo in it&#39;s root directory ([`c67aef4`](https://github.com/Riminder/hrflow-connectors/commit/c67aef410a9e56b0672f08c0beedd17e7ba81f04))

### Documentation

* docs: update main readme ([`a355f68`](https://github.com/Riminder/hrflow-connectors/commit/a355f68dc6e799f5ac4bdb35062307d3cfab4710))

### Feature

* feat: enforce square logo and update non conforming logos ([`8919753`](https://github.com/Riminder/hrflow-connectors/commit/8919753eeeb67fd745f76bf886fb6f9304bb88c9))

### Fix

* fix: use square logo for tests and add test for square logo check ([`331fa37`](https://github.com/Riminder/hrflow-connectors/commit/331fa37c1d41815ba155516db8d08318b09ef41c))

* fix: use lstat rather than state(follow_symlinks=False)
follow_symlinks is only available from py3.10 ([`3f05814`](https://github.com/Riminder/hrflow-connectors/commit/3f05814dc8fca9f3267b718530b39d767a954e56))

### Unknown

* doc: update manifest with logo links ([`0fdd3a7`](https://github.com/Riminder/hrflow-connectors/commit/0fdd3a76a9165ba749a1c50e84a25ac64b7f36c7))

* Adding logos to Connectors ([`79f1378`](https://github.com/Riminder/hrflow-connectors/commit/79f137841413c6a877651272fa7d74162bd8c633))

* doc: update ConnectorType
&#39;Automation Tools&#39; -&gt; &#39;Automation&#39;
&#39;HCM Cloud&#39; -&gt; &#39;HCM&#39;
Correct SAPSuccessFactors, Leboncoin, Twilio ... ([`bec76b4`](https://github.com/Riminder/hrflow-connectors/commit/bec76b4e8dd364549bdda6c2f69488bd00d62d84))


## v3.2.0 (2023-10-05)

### Ci

* ci: allow peter-evans/repository-dispatch@v2.1.2 Github Action ([`cbd945a`](https://github.com/Riminder/hrflow-connectors/commit/cbd945ae258e7ef7554652939f920f337f3f82bc))

* ci: rely on outputs of semantic-release command for jobs orchestration ([`536c56e`](https://github.com/Riminder/hrflow-connectors/commit/536c56e71b3a381b47b4baefb2b6a9d30f48ea18))

* ci: dispatch event to update workflows2.0 on new release ([`0b57030`](https://github.com/Riminder/hrflow-connectors/commit/0b57030165ecd129ac480ffe26a214257dccebc3))

* ci: use same steps order for connectors-integration-tests job trying to solve nox command not found error ([`c106d99`](https://github.com/Riminder/hrflow-connectors/commit/c106d999e2255ec8f334a945a0d8ad33a86e9566))

* ci: replace old author_association logic with protected Github Environments ([`9565e80`](https://github.com/Riminder/hrflow-connectors/commit/9565e80fb63f60b546929358d8ff6cd74c70d8aa))

### Documentation

* docs: correct last update date for Bulhorn ([`95ab01d`](https://github.com/Riminder/hrflow-connectors/commit/95ab01d74ca2b0ede1fa71fc5634c1c4ffb81b22))

### Feature

* feat: add &#39;_&#39; as valid token around date in main readme listing

It seems that auto formatting of markdown changes the actual &#39;*&#39; to &#39;_&#39;
which causes make docs to fail. This fixes the issue by updating the finder regexp ([`4374899`](https://github.com/Riminder/hrflow-connectors/commit/43748992d051136d431cd762e907ca488beaacb4))

### Unknown

* Update Bullhorn Connector Doc ([`d246be3`](https://github.com/Riminder/hrflow-connectors/commit/d246be3464b23dff2a12dda8bb19788235a14107))


## v3.1.1 (2023-09-18)

### Fix

* fix: use BASE_CONNECTOR_PATH context var for readme_link ([`a131982`](https://github.com/Riminder/hrflow-connectors/commit/a131982da86b9617beeea784b9b6a9bb5641ccef))


## v3.1.0 (2023-09-18)

### Ci

* ci: Add missing loading cached venv in integration-tests job ([`778d825`](https://github.com/Riminder/hrflow-connectors/commit/778d8256e04e7f3cc3b034a6bec12eb784984797))

* ci: correct permissions for integration-tests ([`2601883`](https://github.com/Riminder/hrflow-connectors/commit/26018836c8d7a2ae9e66b984569412075d05a7ad))

### Documentation

* docs: add link to automatic version bumping section of Python Packages e-book ([`b19f801`](https://github.com/Riminder/hrflow-connectors/commit/b19f8016f9652cce2c44952ed332f1fa0f0cf784))

* docs: add details about how python-semantic-release is used to enable automatic versionning ([`9d3e7fd`](https://github.com/Riminder/hrflow-connectors/commit/9d3e7fd07812d314542793a796ff200da97a9c29))

* docs: customize semantic-release changelog template to only list releases since 18/09/2023 ([`f6f7b60`](https://github.com/Riminder/hrflow-connectors/commit/f6f7b602aad4a74584b401714c326661c9984061))

* docs: remove duplicated comment ([`0f82c75`](https://github.com/Riminder/hrflow-connectors/commit/0f82c7513b7d764dedd2fe14bff4bcfeba618388))

### Feature

* feat: allow connectors path to be supplied for git update process
This allow other repositories to use the main Readme listing function
while properly linking to their connectors ([`165f1ac`](https://github.com/Riminder/hrflow-connectors/commit/165f1ac8b8e47c3cc58bc0f55297e1b5cbcacb1f))
