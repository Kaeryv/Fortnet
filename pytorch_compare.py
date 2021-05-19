from tensorflow.keras.layers import Dense, Input, Flatten
from tensorflow.keras.models import Sequential
from tensorflow import keras
import numpy as np

# Let's load the dataset
from tensorflow.keras.datasets.mnist import load_data
num_classes = 10

(x_train, y_train), (x_test, y_test)  = load_data()
x_train = np.expand_dims(x_train.astype("float32"),-1) / 255.
x_test = np.expand_dims(x_test.astype("float32"),-1) / 255.
y_train = keras.utils.to_categorical(y_train, num_classes)
y_test = keras.utils.to_categorical(y_test, num_classes)
from matplotlib import pyplot as plt
plt.imshow(x_train[1])

model = Sequential()
model.add(Input(shape=(28,28,1)))
model.add(Flatten())
l1 = Dense(128, activation="relu", use_bias=False)
model.add(l1)
model.add(Dense(128, activation="tanh", use_bias=False))
model.add(Dense(128, activation="tanh", use_bias=False))

model.add(Dense(10, activation="softmax", use_bias=False))

model.compile(loss="mse", optimizer="sgd", metrics=["accuracy"])
plt.imshow(l1.get_weights()[0].T)


model.fit(x_train, y_train, batch_size=32, epochs=5)
