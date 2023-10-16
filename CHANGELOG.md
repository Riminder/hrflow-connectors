# CHANGELOG



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
