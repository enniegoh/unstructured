# audio analysis

library(seewave)
library(tuneR)
library(tidyverse)
library(audio)
library(fftw)
library(ggplot2)
library(plotly)
library(cowplot)

###################################### Cat #############################################
# Import data
audio1 <- readWave("meow1.wav")
scale_audio1 <- normalize(audio1, unit = c("1"), center = F, rescale = F)
summary(scale_audio1)

# Waveform signal
y1 <- scale_audio1@left
Fs1 <- scale_audio1@samp.rate
wvfm1 <- ggplot(mapping = aes(x = seq_len(length(y1)), y = y1)) +
  geom_line(color = "seagreen") +
  labs(x = "Sample Number", y = "Amplitude", title = "Cat Audio Waveform")
print(wvfm1)

# Spectrogram
N1 <- 32e-3 * Fs1
spect1 <- ggspectro(y1, Fs1, wl = N1, wn = "hamming", ovlp = 50, fftw = T) +
  geom_tile(aes(fill = amplitude)) +
  scale_fill_viridis_c()
print(spect1)

# FFT
dft1 <- FFT(y1)
dft1_amp <- abs(dft1)
n1 <- length(y1)
freq1 <- (seq_len(n1) - 1) * Fs1/n1
freq_plot1 <- ggplot(mapping = aes(x = freq1[1:Fs1], y = dft1_amp[1:Fs1])) +
  geom_line(color = "seagreen") +
  labs(x = "Frequency (Hz)", y = "Magnitude", title = "Frequency domain representation of Cat Audio")
print(freq_plot1)

# remove symmetrical problem
half_Fs1 <- Fs1/2
half_freq_plot1 <- ggplot(mapping = aes(x = freq1[1:Fs1], y = dft1_amp[1:Fs1])) +
  geom_line(color = "seagreen") +
  labs(x = "Frequency (Hz)", y = "Magnitude", title = "Frequency domain representation of Cat Audio") +
  scale_x_continuous(limits = c(0, half_Fs1))
print(half_freq_plot1)
ggplotly(half_freq_plot1)



###################################### Dog #############################################
# Import data
audio2 <- readWave("chien.wav")
scale_audio2 <- normalize(audio2, unit = c("1"), center = F, rescale = F)
summary(scale_audio2)

# Waveform signal
y2 <- scale_audio2@left
Fs2 <- scale_audio2@samp.rate
wvfm2 <- ggplot(mapping = aes(x = seq_len(length(y2)), y = y2)) +
  geom_line(color = "violet") +
  labs(x = "Sample Number", y = "Amplitude", title = "Dog Audio Waveform")
print(wvfm2)
ggplotly(wvfm2)

# Spectrogram
N2 <- 32e-3 * Fs2
spect2 <- ggspectro(y2, Fs2, wl = N2, wn = "hamming", ovlp = 50, fftw = T) +
  geom_tile(aes(fill = amplitude)) +
  scale_fill_viridis_c()
print(spect2)

# FFT
dft2 <- FFT(y2)
dft2_amp <- abs(dft2)
n2 <- length(y2)
freq2 <- (seq_len(n2) - 1) * Fs2/n2
freq_plot2 <- ggplot(mapping = aes(x = freq2[1:Fs2], y = dft2_amp[1:Fs2])) +
  geom_line(color = "violet") +
  labs(x = "Frequency (Hz)", y = "Magnitude", title = "Frequency domain representation of Dog Audio")
print(freq_plot2)

## remove symmetrical problem
half_Fs2 <- Fs2/2
half_freq_plot2 <- ggplot(mapping = aes(x = freq2[1:Fs2], y = dft2_amp[1:Fs2])) +
  geom_line(color = "violet") +
  labs(x = "Frequency (Hz)", y = "Magnitude", title = "Frequency domain representation of Dog Audio") +
  scale_x_continuous(limits = c(0, half_Fs2))
print(half_freq_plot2)
ggplotly(half_freq_plot2)
