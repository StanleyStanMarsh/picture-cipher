# picture-cipher (EN)

This project is an implementation of the `Lib.hs` library, which includes functions for encrypting text using the Caesar Cipher method and subsequently hiding the encrypted text within a 24-bit .bmp image.

## Key Features

- **Text Encryption**: The user inputs a shift value for the Caesar Cipher method, after which the text from a specified .txt file is encrypted.
- **Steganography**: The encrypted text is embedded into a .bmp image. The encryption key is saved in the image file's name.
- **Decryption**: The image with the embedded text can be decoded back into a text file using the key extracted from the file name.

## Installation and Execution

To run the project, you will need a Haskell compiler (e.g., GHC) and the `stack` build tool.

### Compilation and Execution

1. Open a terminal and navigate to the project directory.
2. Compile the program using the command:
```bash
   stack build
```
3. Run the application:
```bash
   stack exec picture-cipher-exe
```

### Using the Program

After launching, the program will prompt you to choose a mode of operation:
- Encrypt and embed text into an image.
- Decrypt text from an image.

User will be prompted to enter the necessary parameters (shift value for encryption, path to the source text file, path to the image, etc.).

Depending on the number of encoded (last) bits in each byte of the image, the following results are obtained (Fig. 1-8).

![1](https://github.com/user-attachments/assets/d01dcd0a-6eca-4465-8889-15ff67a41758 "Fig. 1")
![2](https://github.com/user-attachments/assets/a98a0c43-7d9e-41c1-94cb-97b0f820e71c "Fig. 2")
![3](https://github.com/user-attachments/assets/ab429391-0dda-405c-bff0-bfbae3682078 "Fig. 3")
![4](https://github.com/user-attachments/assets/5575c24b-9ff3-40bb-9409-23c82c646ce1 "Fig. 4")
![5](https://github.com/user-attachments/assets/c387dfe5-41e8-4685-bca0-e68dd3699e57 "Fig. 5")
![6](https://github.com/user-attachments/assets/d560b02a-fa57-4848-a98a-5b73bb3923c2 "Fig. 6")
![7](https://github.com/user-attachments/assets/78b0c954-2454-4905-bc7c-9fb8af5aa67e "Fig. 7")
![8](https://github.com/user-attachments/assets/27bb0da1-7c50-4e65-ad1f-f56a257a7710 "Fig. 8")

## Project Structure

- `Lib.hs`: A library of pure functions for text encryption/decryption and steganography.
- `Main.hs`: The main module that manages user interaction and calls the corresponding functions from `Lib.hs`.
- `sample.txt`: An example input text file.
- `sample.bmp`: An example image for embedding text.

## License

This project is distributed under the MIT License. You are free to use, modify, and distribute the code according to the terms of this license.

# picture-cipher (RU)

Этот проект представляет собой реализацию библиотеки Lib.hs, которая включает функции для шифрования текста методом Шифра Цезаря и последующего сокрытия зашифрованного текста внутри изображения формата .bmp (24-разрядный).

## Основные возможности

- Шифрование текста: Пользователь вводит смещение для метода Шифра Цезаря, после чего текст из указанного файла .txt шифруется.
- Стеганография: Зашифрованный текст внедряется в изображение .bmp. Ключ к шифру сохраняется в имени файла изображения.
- Расшифровка: Изображение с внедренным текстом может быть декодировано обратно в текстовый файл при помощи ключа, извлеченного из имени файла.

## Установка и запуск

Для запуска проекта вам потребуется Haskell-компилятор (например, GHC) и программа сборки stack.

### Компиляция и выполнение

1. Откройте терминал и перейдите в директорию проекта.
2. Скомпилируйте программу командой:
```bash
   stack build
```
3. Запустите полученное исполняемое приложение:
```bash
   stack exec picture-cipher-exe
```

### Использование программы

После запуска программа предложит вам выбрать режим работы:
- Шифрование и внедрение текста в изображение.
- Расшифровка текста из изображения.

Пользователю будет предложено ввести необходимые параметры (смещение для шифрования, путь к исходному текстовому файлу, путь к изображению и т.п.).

В зависимости от количества кодируемых (последних) битов в каждом байте изображения получаются следующие результаты (Рис. 1-8).

![1](https://github.com/user-attachments/assets/d01dcd0a-6eca-4465-8889-15ff67a41758 "Рис. 1")
![2](https://github.com/user-attachments/assets/a98a0c43-7d9e-41c1-94cb-97b0f820e71c "Рис. 2")
![3](https://github.com/user-attachments/assets/ab429391-0dda-405c-bff0-bfbae3682078 "Рис. 3")
![4](https://github.com/user-attachments/assets/5575c24b-9ff3-40bb-9409-23c82c646ce1 "Рис. 4")
![5](https://github.com/user-attachments/assets/c387dfe5-41e8-4685-bca0-e68dd3699e57 "Рис. 5")
![6](https://github.com/user-attachments/assets/d560b02a-fa57-4848-a98a-5b73bb3923c2 "Рис. 6")
![7](https://github.com/user-attachments/assets/78b0c954-2454-4905-bc7c-9fb8af5aa67e "Рис. 7")
![8](https://github.com/user-attachments/assets/27bb0da1-7c50-4e65-ad1f-f56a257a7710 "Рис. 8")

## Структура проекта

- Lib.hs: Библиотека чистых функций для шифрования/дешифрования текста и работы со стеганографией.
- Main.hs: Основной модуль, который управляет взаимодействием с пользователем и вызывает соответствующие функции из Lib.hs.
- sample.txt: Пример входного текстового файла.
- sample.bmp: Пример изображения для внедрения текста.

## Лицензия

Этот проект распространяется под лицензией MIT. Вы можете использовать, изменять и распространять код согласно условиям этой лицензии.
