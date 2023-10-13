from enum import Enum


class WorkdayErrorCode(str, Enum):
    A240 = "Enter end year when you enter the end month"
    A242 = "Enter only 1 ability type for the same language"
    A246 = (
        "One of the following file extensions must be used: [allowed file extensions]"
    )
    A247 = "Enter a title"
    A249 = "You must enter the prospect's email address or phone number"
    A252 = "Enter a file name"
    A253 = "Enter a number from 1 to 12 for end month"
    A255 = "Enter referral as the source when you enter referred by"
    A256 = "Enter a name"
    A257 = "Enter a skill name"
    A258 = "Enter a start year"
    A259 = "You cannot enter duplicate skills"
    A261 = "Enter a school name"
    A263 = "Enter a language"
    A267 = "At least one ability for this language is required"
    A268 = "Enter the referral for the prospect"
    A269 = "Enter an email address in this format: xxx@yy.com"
    A270 = "Select a file to upload"
    A271 = (
        "Enter an end month and end year that occur on or after the start month and"
        " start year"
    )
    A274 = "Enter a valid format for Phone Number"
    A278 = "Enter active candidate pools"
    A282 = "Enter a company name"
    A283 = "Enter a last year attended that is after the first year attended"
    A284 = "Enter active candidate tags"
    A286 = "Enter a number from 1 to 12 for start month"
    A288 = "Enter a phone number in the valid format"
    A290 = "Duplicate language entries are not allowed"
    A673 = "The Phone Device Type that you specified must be Active"
    A820 = "Specify a value for Phone Device Type"
    A821 = "You can't specify a Phone Device Type that's Hidden for Recruiting"
    A822 = "Specify a value for Phone Number"
    A823 = "Specify a value for Country Phone Code"
