import typing as t
from logging import LoggerAdapter

import mysql.connector
from mysql.connector import Error
from mysql.connector.connection import MySQLConnection


def connect_to_database(host, port, user, password, database):
    try:
        connection = mysql.connector.connect(
            host=host, port=port, user=user, password=password, database=database
        )
        if connection.is_connected():
            print("Connected to the database")
            return connection
    except Error as e:
        print(f"Error: {e}")
        return None


# Function to query the database and return results as a dictionary
def query_database(connection, query):
    cursor = connection.cursor(dictionary=True)
    cursor.execute(query)
    results = cursor.fetchall()
    cursor.close()
    return results


# Function to query the database with pagination and return results as a dictionary
def query_database_with_pagination(connection, query, offset, limit):
    paginated_query = f"{query} LIMIT {limit} OFFSET {offset}"
    cursor = connection.cursor(dictionary=True)
    cursor.execute(paginated_query)
    results = cursor.fetchall()
    cursor.close()
    return results


# Function to insert a new object into a table
def insert_object(
    adapter: LoggerAdapter,
    connection: MySQLConnection,
    table_name: str,
    object_data: t.Dict,
) -> t.Optional[int]:
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
