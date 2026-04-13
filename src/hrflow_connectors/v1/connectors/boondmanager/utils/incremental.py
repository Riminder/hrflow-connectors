import json
import typing as t


def to_api_date(iso_date: str) -> t.Optional[str]:
    """Convert BoondManager ISO datetime to the API startDate format.

    Example: "2026-04-07T17:57:03+0200" → "2026-04-07 17:57:03"
    """
    if not iso_date:
        return None
    return iso_date[:19].replace("T", " ")


def item_to_read_from(item: dict) -> str:
    """Build a JSON cursor string from a BoondManager item.

    Stores both ``updateDate`` and the BoondManager ``id`` so the
    tiebreaker can skip items already processed at the cursor boundary.
    """
    return json.dumps(
        dict(
            update_date=item.get("attributes", {}).get("updateDate", ""),
            last_id=str(item.get("id", "")),
        )
    )


def parse_cursor(read_from: str) -> t.Tuple[t.Optional[str], t.Optional[str]]:
    """Parse an incremental cursor into (last_update_date, last_id).

    Handles the current JSON format and falls back to a plain ISO date
    string for backwards compatibility.
    """
    try:
        cursor = json.loads(read_from)
        return cursor.get("update_date"), cursor.get("last_id")
    except (json.JSONDecodeError, AttributeError):
        return read_from, None


def should_skip_item(
    item: dict,
    last_update_date: t.Optional[str],
    last_id: t.Optional[str],
) -> bool:
    """Return True if *item* was already processed in the previous run.

    The tiebreaker logic: skip items whose ``updateDate`` equals the
    cursor date **and** whose BoondManager ``id`` is ≤ the cursor id.
    Items with a strictly newer ``updateDate`` are never skipped.
    """
    if not last_update_date or not last_id:
        return False
    return (
        item.get("attributes", {}).get("updateDate") == last_update_date
        and str(item.get("id", "")) <= last_id
    )
