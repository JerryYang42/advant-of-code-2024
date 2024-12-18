"""
This file has to be put under src/main/python/ in order to route the path to the root directory of the project.
"""

import os
from typing import Final

PYTHON_DIR: Final[str] = os.path.dirname(os.path.abspath(__file__))
MAIN_DIR: Final[str] = os.path.dirname(PYTHON_DIR)
RESOURCES_DIR: Final[str] = os.path.join(MAIN_DIR, "resources")
SRC_DIR: Final[str] = os.path.dirname(MAIN_DIR)
TEST_DIR: Final[str] = os.path.join(SRC_DIR, "test")


if __name__ == "__main__":
    assert(PYTHON_DIR.endswith("src/main/python"))
    assert(MAIN_DIR.endswith("src/main"))
    assert(RESOURCES_DIR.endswith("src/main/resources"))
    assert(SRC_DIR.endswith("src"))
    assert(TEST_DIR.endswith("src/test"))
