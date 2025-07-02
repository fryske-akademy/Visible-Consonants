# Visible-Consonants
Software for analyzing acoustic consonant measurements. The app is an useful instrument for research in phonetics, sociolinguistics, dialectology, forensic linguistics, and speech-language pathology.

Follow the instructions below to build the Docker image and launch the container.

### 1. Clone the Repo

```
git clone https://github.com/fryske-akademy/Visible-Consonants.git
cd Visible-Consonants
```

### 2. Build the Docker Image

```
docker build -t visible-consonants .
```

### 3. Run the Container

```
docker run -p 3838:3838 visible-consonants
```

### 4. View in Browser

Open:
http://localhost:3838
