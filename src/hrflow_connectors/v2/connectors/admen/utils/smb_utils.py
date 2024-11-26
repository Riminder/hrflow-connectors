import os
import time
from logging import LoggerAdapter
from typing import Optional, Tuple

from smbprotocol.connection import Connection
from smbprotocol.session import Session
from smbprotocol.tree import TreeConnect


def establish_smb_connection(
    adapter: LoggerAdapter,
    server: str,
    username: str,
    password: str,
    share_name: str,
    max_retries: int = 5,
    retry_delay: int = 5,
) -> Tuple[bool, Optional[TreeConnect], Optional[Session], Optional[Connection]]:
    """
    Establish an SMB connection to the specified share.

    Returns a tuple containing:
        - smb_connected (bool): Whether the connection was successful.
        - smb_tree (TreeConnect): SMB tree connection object.
        - smb_session (Session): SMB session object.
        - smb_connection (Connection): SMB connection object.
    """
    for retries in range(max_retries):
        try:
            # Establish a connection to the SMB server
            smb_connection = Connection(
                guid=os.urandom(16),
                server_name=server,
                port=445,
            )
            smb_connection.connect()

            # Create and authenticate a session
            smb_session = Session(
                smb_connection,
                username,
                password,
            )
            smb_session.connect()

            # Connect to the specified share
            smb_tree = TreeConnect(
                smb_session,
                f"\\\\{server}\\{share_name}",
            )
            smb_tree.connect()

            adapter.info("SMB connection established successfully.")
            return True, smb_tree, smb_session, smb_connection
        except Exception as e:
            adapter.error(f"SMB connection error: {e}")
            if retries < max_retries - 1:
                adapter.error(
                    f"Retrying SMB connection in {retry_delay} seconds... "
                    f"({retries + 1}/{max_retries})"
                )
                time.sleep(retry_delay)
            else:
                adapter.error("Max SMB connection retries reached.")
    return False, None, None, None
