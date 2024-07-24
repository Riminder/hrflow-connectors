import mysql.connector
from mysql.connector import Error


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
def insert_object(connection, table_name, object_data):
    placeholders = ", ".join(["%s"] * len(object_data))
    columns = ", ".join(object_data.keys())
    sql = f"INSERT INTO {table_name} ({columns}) VALUES ({placeholders})"
    cursor = connection.cursor()
    try:
        cursor.execute(sql, list(object_data.values()))
        connection.commit()
        print("Object inserted successfully")
    except Error as e:
        connection.rollback()
        print(f"Error: {e}")
    finally:
        cursor.close()
