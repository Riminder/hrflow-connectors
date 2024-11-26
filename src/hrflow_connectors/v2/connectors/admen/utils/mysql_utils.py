import typing as t
from logging import LoggerAdapter

import mysql.connector
from mysql.connector import Error
from mysql.connector.connection import MySQLConnection


def connect_to_database(
    host: str, port: int, user: str, password: str, database: str
) -> t.Optional[MySQLConnection]:
    """Establish a connection to the database.

    :param host: The hostname of the database server.
    :param port: The port number to connect to.
    :param user: The username to authenticate with.
    :param password: The password to authenticate with.
    :param database: The name of the database to connect to.
    :return: A MySQLConnection object if the connection was successful, otherwise None.
    """
    try:
        connection = mysql.connector.connect(
            host=host, port=port, user=user, password=password, database=database
        )
        if isinstance(connection, MySQLConnection) and connection.is_connected():
            print("Connected to the database")
            return connection
    except Error as e:
        print(f"Error: {e}")
    return None


# Function to query the database and return results as a dictionary
def query_database(
    connection: MySQLConnection, query: str, params: t.Optional[t.Tuple] = None
) -> t.Optional[t.List[t.Dict]]:
    """
    Execute a query with optional parameters and return results as a dictionary.

    :param connection: Database connection object.
    :param query: SQL query string.
    :param params: Tuple or list of parameters to use in the query.
    :return: List of dictionaries containing the query results.
    """
    cursor = connection.cursor(dictionary=True)
    try:
        if params:
            cursor.execute(query, params)
        else:
            cursor.execute(query)
        results = cursor.fetchall()
    except Exception as e:
        print(f"Error executing query: {e}")
        results = []
    finally:
        cursor.close()
    return results


# Function to insert a new object into a table
def insert_object(
    adapter: LoggerAdapter,
    connection: MySQLConnection,
    table_name: str,
    object_data: t.Dict,
) -> t.Optional[int]:
    """
    Inserts a new object into the database.

    :param adapter: LoggerAdapter for logging.
    :param connection: MySQL database connection.
    :param table_name: The name of the table to insert into.
    :param object_data: Dictionary of column-value pairs to insert.
    :return: The ID of the newly inserted row, or None if the operation failed.
    """

    placeholders = ", ".join(["%s"] * len(object_data))
    columns = ", ".join(
        [f"`{col}`" for col in object_data.keys()]
    )  # Enclose column names in backticks
    sql = f"INSERT INTO {table_name} ({columns}) VALUES ({placeholders})"
    cursor = connection.cursor()
    try:
        cursor.execute(sql, list(object_data.values()))
        connection.commit()
        inserted_id = cursor.lastrowid  # Get the ID of the newly inserted row
        adapter.info("Object inserted successfully with ID:", inserted_id)
        return inserted_id  # Return the ID to the caller
    except Error as e:
        connection.rollback()
        adapter.error(f"Failed to insert object into the table {table_name}: {e}")
        return None
    finally:
        cursor.close()


def update_object(
    adapter: LoggerAdapter,
    connection: MySQLConnection,
    table_name: str,
    object_data: t.Dict,
    where_clause: t.Dict,
) -> bool:
    """
    Updates an object in the database.

    :param adapter: LoggerAdapter for logging.
    :param connection: MySQL database connection.
    :param table_name: The name of the table to update.
    :param object_data: Dictionary of column-value pairs to update.
    :param where_clause: Dictionary of conditions to identify the rows to update.
    :return: True if the operation was successful, False otherwise.
    """
    set_clause = ", ".join([f"`{col}` = %s" for col in object_data.keys()])
    where_conditions = " AND ".join([f"`{col}` = %s" for col in where_clause.keys()])
    sql = f"UPDATE {table_name} SET {set_clause} WHERE {where_conditions}"

    cursor = connection.cursor()
    try:
        cursor.execute(sql, list(object_data.values()) + list(where_clause.values()))
        connection.commit()
        rows_affected = cursor.rowcount
        adapter.info(f"{rows_affected} rows updated in table {table_name}.")
        return True
    except Error as e:
        connection.rollback()
        adapter.error(f"Failed to update object in the table {table_name}: {e}")
        return False
    finally:
        cursor.close()


# def update_object(
#     adapter: LoggerAdapter,
#     connection: MySQLConnection,
#     table_name: str,
#     object_data: t.Dict,
#     where_clause: t.Dict,
# ) -> bool:
#     set_clause = ", ".join(
#         [
#             f"`{col}` = %s" if object_data[col] is not None else f"`{col}` = NULL"
#             for col in object_data
#         ]
#     )
#     where_conditions = " AND ".join(
#         [
#             f"`{col}` = %s" if where_clause[col] is not None else f"`{col}` IS NULL"
#             for col in where_clause
#         ]
#     )
#     sql = f"UPDATE {table_name} SET {set_clause} WHERE {where_conditions}"

#     cursor = connection.cursor()
#     try:
#         values = [value for value in object_data.values() if value is not None] + [
#             value for value in where_clause.values() if value is not None
#         ]
#         adapter.debug("SQL Query:", sql)
#         adapter.debug("Values:", values)
#         cursor.execute(sql, values)
#         connection.commit()
#         rows_affected = cursor.rowcount
#         if rows_affected == 0:
#             adapter.warning(f"No rows updated in table {table_name}.")
#         else:
#             adapter.info(f"{rows_affected} rows updated in table {table_name}.")
#         return rows_affected > 0
#     except Error as e:
#         connection.rollback()
#         adapter.error(f"Failed to update object in the table {table_name}: {e}")
#         return False
#     finally:
#         cursor.close()
