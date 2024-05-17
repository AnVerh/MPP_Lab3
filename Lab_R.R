# необхідні бібліотеки
library(stringr)
library(data.table)

# Функція, що знаходить параметри апроксимації бета-розподілу: alpha, beta, loc, scale
fit_beta <- function(sorted_data) {
  n <- length(sorted_data)
  sum_x <- sum(sorted_data)
  sum_log_x <- sum(log(sorted_data))
  
  alpha <- (n * sum_log_x + (n - 1) * sum(log(sorted_data[n]))) / (n * log(sorted_data[n]) - sum_log_x)
  beta <- (n * log(sorted_data[n]) + (n - 1) * sum_log_x) / (n * log(sorted_data[n]) - sum_log_x)
  
  loc <- sorted_data[1]
  scale <- sorted_data[n] - loc
  
  return(list(abs(alpha), abs(beta), loc, scale))
}

#функція розподілу значень на інтервали
beta_interval_partition <- function(data, alphabet) {
  alphabet_size <- length(alphabet)
  
  # Сортуємо значення
  sorted_data <- sort(data)

  # Знаходимо параметри бета-розподілу відповідно до даних
  params <- fit_beta(sorted_data)
  alpha <- params[[1]]
  beta <- params[[2]]
  loc <- params[[3]]
  scale <- params[[4]]
  
  cat("Alpha:", alpha, "\n")
  cat("Beta:", beta, "\n")
  
  # Знаходимо кумулятивну ймовірність для кожного значення
  cumulative_probabilities <- pbeta(sorted_data, alpha, beta, loc, scale)
  cat("Cumulative Probabilities:", cumulative_probabilities, "\n")
  
  # Формуємо інтервали відповідно до кумулятивних ймовірностей
  interval_indices <- ceiling(cumulative_probabilities * alphabet_size)
  cat("Interval indices:", interval_indices, "\n")
  
  # Функція створення словника з інтервалами
  create_interval_dict <- function(indices, values) {
    dict <- list()
    populate_dict <- function(indices, values, dict) {
      if (length(indices) == 0) {
        return(dict)  # Повертаємо словник к
      } else {
        idx <- indices[1]  # Беремо перший індекс
        value <- values[1]  # Беремо відповідне значення
        rest_indices <- indices[-1]  # Беремо індекси, що залишилися
        rest_values <- values[-1]  # Беремо значення, що залишилися
      
        dict[[as.character(idx)]] <- c(dict[[as.character(idx)]], value) # створюємо словник
      
        return(populate_dict(rest_indices, rest_values, dict))
      }
    }
  
  # Викликаємо рекурсивну функцію, щоб заповнити словник значеннями
    return(populate_dict(indices, values, dict))
  }
  interval_dict <- create_interval_dict(interval_indices, sorted_data)

  # Визначаємо символ для кожного інтервалу
  assigned_symbols <- setNames(alphabet, seq_along(alphabet))
  # створюємо кінцевий словник - символи до яких прив'язані значення на інтервалі
  result <- list()  #
  for (key in names(interval_dict)) {
    value <- interval_dict[[key]]
    result[[assigned_symbols[[key]]]] <- value
  }

  return(result)
}

# створюємо дані та алфавіт
data <- rbeta(10, 5, 2)
alphabet_size <- 3
alphabet <- LETTERS[1:alphabet_size]

# Знаходимо інтервали за бета-розподілом
intervals <- beta_interval_partition(data, alphabet)

for (key in names(intervals)) {
  values <- intervals[[key]]
  cat("Interval", key, ":", toString(values), "\n")
}
# Ініціалізуємо лінгвістичний ланцюжок
letter_data <- ""

# Формуємо лінгвістичний ланцюжок
for (number in data) {
  for (interval in names(intervals)) {
    letter <- interval
    numbers <- intervals[[interval]]
    if (number %in% numbers) {
      letter_data <- str_c(letter_data, letter)
    }
  }
}

# Друкуємо початкові дані та лінгвістичний ланцюжок
cat("Початкові дані:")
cat(paste(data, collapse = " "))
cat("\nЛінгвістичний ряд: \n")
cat(letter_data)

# Створюємо матрицю передування
ling_matrix <- matrix(0, nrow = length(alphabet), ncol = length(alphabet))
for (i in 1:length(alphabet)) {
  for (j in 1:length(alphabet)) {
    row_letter <- alphabet[i]
    col_letter <- alphabet[j]
    seeking_letters <- paste0(row_letter, col_letter)
    appear <- 0
    
    # Ітеруємо через кожну пару символів, щоб знайти кількість випадків передування
    for (l in 1:(nchar(letter_data) - 1)) {
      if (str_sub(letter_data, l, l + 1) == seeking_letters) {
        appear <- appear + 1
      }
    }
    # Приписуємо значенню матриці кількість випадків передування
    ling_matrix[i, j] <- appear
  }
}

# Друкуємо матрицю передування
cat("\nМатриця передування:\n")
print(ling_matrix)

