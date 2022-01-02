# Changelog
All notable changes to this project will be documented in this file.

## Version 0.1.0 - 2021/12/07
### Added
* The basic architecture
* `Action`
* `BoardAction`
* `HTTPStream`
* `OAuth2PasswordCredentialsBody`
* Crosstalent `GetAllJobs` Connector
* Logics
### Changed
* Move old files to`legacy/`

## Version 0.2.0 - 2022/01/02
### Added
* Craigslist `CraigslistFeed` Connector
* Crosstalent `PushProfile` Connector
* Flatchr `PushProfile` Connector
* Flatchr `EnrichProfile` Connector
* Indeed `IndeedFeed` Connector
* Smartrecruiters `SmartJobs` Connector
* `hydrate_with_parsing` in `BoardAction`
* `hydrate_with_parsing` in `BoardAction`
* Logging
* `format_function_name` : use external function to format data
* `archive_deleted_jobs_from_stream` : Automatically archive a job when it is deleted from the stream
* `EventParser` with `get_profile`
* `Profile` and `Source` objects
* `XAPIKeyAuth`
* `AuthorizationAuth`
* `XSmartTokenAuth`
* `from_str_to_datetime` : Convert string to `datetime.datetime`
* `from_str_to_timedelta` : Convert string to `datetime.timedelta`
* `remove_html_tags` : Remove all HTML tags in a string
* `find_element_in_list` : Find element with some fields in list of elements
* `generate_workflow_response` : Generate CATCH workflow response
* Dependencies : `selenium`
### Changed
* Pipeline in Action and BoardAction to use `format_switcher` instead of `format`
### Fixed
* HTTPStream : static private attributes become public attributes