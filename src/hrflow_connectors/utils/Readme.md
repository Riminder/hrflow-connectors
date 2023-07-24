# HrFlow Connectors - Utils Section

Welcome to the Utils section of HrFlow Connectors! Here you will find a collection of convenient utility functions that can be utilized within the connectors or independently to perform various tasks, including geolocation from location text and more.

## Geolocation

The geolocation utility allows you to obtain latitude and longitude information from city names or postal codes. This can be particularly useful when dealing with location-based data and services.

Please note that at the moment, geolocation is only supported for locations in France, but we are continuously working on expanding its capabilities and adding improvements.


The utility is designed to be easy to use, as shown in the example below:

### Example Usage

```python
from hrflow_connectors.utils.geolocation import get_geolocation_data

location_string = "Neuilly-sur-Seine"

geolocation_data = get_geolocation_data(location=location_string)
print(geolocation_data)
```

Please note that the example provided above assumes you have imported the `get_geolocation_data` function from the appropriate module. The function will return the geolocation data for the specified location in a structured format.

## Contributing

We appreciate your interest in contributing to HrFlow Connectors Utils! If you have any improvements, bug fixes, or new utility functions that you'd like to add, please feel free to submit a pull request. We encourage an active and collaborative community that helps make HrFlow Connectors even better.


## Need Help?

If you encounter any issues, have questions, or need assistance with HrFlow Connectors Utils, please don't hesitate to [open an issue](https://github.com/hrflow/your-repo-name/issues). We'll do our best to help you and address any problems you may have.

Thank you for choosing HrFlow Connectors Utils! We hope these utility functions streamline your development process and contribute to the success of your projects. Happy coding!