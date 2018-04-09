from math import sqrt
from numpy import concatenate
from matplotlib import pyplot
from pandas import read_csv
from pandas import DataFrame
from pandas import concat
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import mean_squared_error
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM


# convert series to supervised learning
def series_to_supervised(data, n_in=1, n_out=1, dropnan=True):
    n_vars = 1 if type(data) is list else data.shape[1]
    df = DataFrame(data)
    cols, names = list(), list()
    # input sequence (t-n, ... t-1)
    for i in range(n_in, 0, -1):
        cols.append(df.shift(i))
        names += [('var%d(t-%d)' % (j + 1, i)) for j in range(n_vars)]
    # forecast sequence (t, t+1, ... t+n)
    for i in range(0, n_out):
        cols.append(df.shift(-i))
        if i == 0:
            names += [('var%d(t)' % (j + 1)) for j in range(n_vars)]
        else:
            names += [('var%d(t+%d)' % (j + 1, i)) for j in range(n_vars)]
    # put it all together
    agg = concat(cols, axis=1)
    agg.columns = names
    # drop rows with NaN values
    if dropnan:
        agg.dropna(inplace=True)
    return agg

# load dataset
dataset = read_csv('pollution_imputed.csv', header=0, index_col=0)
values = dataset.values
values.tofile("values_orig.csv", sep=";")
# integer encode direction
encoder = LabelEncoder()
values[:, 4] = encoder.fit_transform(values[:, 4])
values.tofile("values_orig_aftertrafo.csv", sep=";")
# ensure all data is float
values = values.astype('float32')
# normalize features
scaler = MinMaxScaler(feature_range=(0, 1))
scaled = scaler.fit_transform(values)
scaled_df = DataFrame(scaled)
print(scaled_df.head())
#scaled_df.to_csv("scaled.csv", sep=";")
# frame as supervised learning (make a copy of each variable and put them in as a t+1 column)
reframed = series_to_supervised(scaled, 1, 1)
reframed.to_csv("reframed_orig.csv", sep=";")
# drop columns we don't want to predict
reframed.drop(reframed.columns[[9, 10, 11, 12, 13, 14, 15]], axis=1, inplace=True)
print(reframed.head())

# reframed.to_csv("reframed.csv")

# split into train and test sets
values = reframed.values
values.tofile("values.csv", sep=",")
n_train_hours = 365 * 24
train = values[:n_train_hours, :]
test = values[n_train_hours:, :]

# train_panda = DataFrame(train)
# test_panda = DataFrame(test)

# train_panda.to_csv("train_panda.csv")
# test_panda.to_csv("test_panda.csv")

# split into input and outputs
train_X, train_y = train[:, :-1], train[:, -1]
test_X, test_y = test[:, :-1], test[:, -1]
# reshape input to be 3D [samples, timesteps, features]
train_X = train_X.reshape((train_X.shape[0], 1, train_X.shape[1]))
test_X = test_X.reshape((test_X.shape[0], 1, test_X.shape[1]))
print(train_X.shape, train_y.shape, test_X.shape, test_y.shape)

# design network
model = Sequential()
model.add(LSTM(50, input_shape=(train_X.shape[1], train_X.shape[2])))
model.add(Dense(1))
model.compile(loss='mae', optimizer='adam')
# fit network
history = model.fit(train_X, train_y, epochs=1, batch_size=72, validation_data=(test_X, test_y), verbose=2,
                    shuffle=False)

# plot history
pyplot.plot(history.history['loss'], label='train')
pyplot.plot(history.history['val_loss'], label='test')
pyplot.legend()
pyplot.show()

# make a prediction
yhat = model.predict(test_X)

#yhat_panda = DataFrame(yhat)
#yhat_panda.to_csv("yhat.csv")

test_X = test_X.reshape((test_X.shape[0], test_X.shape[2]))
train_X_test = train_X.reshape((train_X.shape[0], train_X.shape[2]))

rmse = sqrt(mean_squared_error(test_y, yhat))
print(mean_squared_error(test_y, yhat))
print('Test RMSE: %.3f' % rmse)
print(DataFrame(test_y).head())
print(DataFrame(test_y).tail())

print(train_y.shape)
print(train_X.shape)
print(test_X.shape)
print(test_y.shape)

y = concatenate((train_y[:, None], train_X_test[:, 1:]), axis=1)
y = scaler.inverse_transform(y)
y = DataFrame(y)
print(y.head())

print(scaler.feature_range)
print(scaler.data_min_)
print(scaler.data_max_)
y = concatenate((test_y[:, None], test_X[:, 1:]), axis=1)
y = scaler.inverse_transform(y)
y = DataFrame(y)
print(len(y))
print(y.head())


# invert scaling for forecast
inv_yhat = concatenate((yhat, test_X[:, 1:]), axis=1)
inv_yhat = scaler.inverse_transform(inv_yhat)
inv_yhat = inv_yhat[:, 0]

# invert scaling for actual
test_y = test_y.reshape((len(test_y), 1))
inv_y = concatenate((test_y, test_X[:, 1:]), axis=1)
inv_y = scaler.inverse_transform(inv_y)
inv_y = inv_y[:, 0]

# calculate RMSE
rmse = sqrt(mean_squared_error(inv_y, inv_yhat))
print(mean_squared_error(inv_y, inv_yhat))
print('Test RMSE: %.3f' % rmse)

