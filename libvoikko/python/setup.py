import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="libvoikko",
    version="4.3",
    author="Harri Pitkänen",
    author_email="hatapitk@iki.fi",
    description="Python API for libvoikko, library of free natural language processing tools",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://voikko.puimula.org/python.html",
    py_modules=['libvoikko'],
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: Mozilla Public License 1.1 (MPL 1.1)",
        "License :: OSI Approved :: GNU General Public License v2 or later (GPLv2+)",
        "License :: OSI Approved :: GNU Lesser General Public License v2 or later (LGPLv2+)",
        "Operating System :: OS Independent"
    ],
    python_requires='>=3.2'
)
